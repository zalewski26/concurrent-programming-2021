package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"
)

var n int         // vertices
var d int         // shortcuts
var max_hosts int // per router
var max_delay = 1000

type address struct {
	router int
	host   int
}

var address_list []address

type vertice struct {
	routing_mutex sync.Mutex
	id            int
	next          []int
	in            chan *pack
	out           []chan *pack
	R             routingTable

	pack_in     chan *standard_pack
	pack_out    []chan *standard_pack
	hosts       []host
	pack_queue  *[]standard_pack
	queue_mutex sync.Mutex
}

type host struct {
	id        int
	router_id int
	in        chan *standard_pack
	out       chan *standard_pack
}

type routingTable struct {
	nextHop []int
	cost    []int
	changed []bool
}

type pack struct {
	senderId int
	nextHop  []int
	cost     []int
	valid    []bool
}

type standard_pack struct {
	sender   address
	receiver address
	visited  []int
}

func generateGraph(verts []vertice) {
	mutual := make(chan *pack)
	mutual2 := make(chan *standard_pack)
	for i := 0; i < n; i++ {
		verts[i].id = i
		verts[i].next = make([]int, 0)
		verts[i].in = mutual
		verts[i].pack_in = mutual2
		temp_queue := make([]standard_pack, 0)
		verts[i].pack_queue = &temp_queue
		mutual = make(chan *pack)
		mutual2 = make(chan *standard_pack)

		verts[i].out = make([]chan *pack, 0)
		verts[i].pack_out = make([]chan *standard_pack, 0)

		verts[i].R.nextHop = make([]int, n)
		verts[i].R.cost = make([]int, n)
		verts[i].R.changed = make([]bool, n)
		for j := 0; j < n; j++ {
			if i == j {
				verts[i].R.nextHop[j] = i
				verts[i].R.cost[j] = 0
				verts[i].R.changed[j] = false
			} else if i < j {
				verts[i].R.nextHop[j] = i + 1
				verts[i].R.cost[j] = j - i
				verts[i].R.changed[j] = true
			} else {
				verts[i].R.nextHop[j] = i - 1
				verts[i].R.cost[j] = i - j
				verts[i].R.changed[j] = true
			}
		}
		if i != n-1 {
			verts[i].next = append(verts[i].next, i+1)
			verts[i].out = append(verts[i].out, mutual)
			verts[i].pack_out = append(verts[i].pack_out, mutual2)

		}
		if i != 0 {
			verts[i].next = append(verts[i].next, i-1)
			verts[i].out = append(verts[i].out, verts[i-1].in)
			verts[i].pack_out = append(verts[i].pack_out, verts[i-1].pack_in)
		}
	}

	i := 0
	for i < d {
		full := true
		for j := 0; j < n; j++ {
			if len(verts[j].next) != n-1 {
				full = false
				break
			}
		}
		if full {
			break
		}

		exists := false
		index := rand.Intn(n - 1)
		new_next := index + 1 + rand.Intn(n-1-index)

		for _, value := range verts[index].next {
			if value == new_next {
				exists = true
				break
			}
		}
		if !exists {
			verts[index].next = append(verts[index].next, new_next)
			verts[index].out = append(verts[index].out, verts[new_next].in)
			verts[index].pack_out = append(verts[index].pack_out, verts[new_next].pack_in)
			verts[index].R.nextHop[new_next] = new_next
			verts[index].R.cost[new_next] = 1

			verts[new_next].next = append(verts[new_next].next, index)
			verts[new_next].out = append(verts[new_next].out, verts[index].in)
			verts[new_next].pack_out = append(verts[new_next].pack_out, verts[index].pack_in)
			verts[new_next].R.nextHop[index] = index
			verts[new_next].R.cost[index] = 1
			i++
		}
	}

	for i := 0; i < n; i++ { // każdy router ma przynajmniej 1 hosta
		numOfHosts := rand.Intn(max_hosts) + 1
		for j := 0; j < numOfHosts; j++ {
			verts[i].hosts = append(verts[i].hosts, host{j, i, make(chan *standard_pack), verts[i].pack_in})
			address_list = append(address_list, address{i, j})
		}
	}
}

func printGraph(verts []vertice) {
	fmt.Println("Graf:")
	for i := 0; i < len(verts); i++ {
		fmt.Print(i, ": [")
		for j := 0; j < len(verts[i].next); j++ {
			if j != 0 {
				fmt.Print(" ")
			}
			fmt.Print(verts[i].next[j])
		}
		fmt.Print("]\n\tHosts: {")
		for j := 0; j < len(verts[i].hosts); j++ {
			if j != 0 {
				fmt.Print(" ")
			}
			fmt.Print(verts[i].hosts[j].id)
		}
		fmt.Println("}")
	}
}

func sender(vert vertice) {
	for {
		time.Sleep(time.Millisecond * time.Duration(300+rand.Intn(max_delay)))
		vert.routing_mutex.Lock()
		tempPack := pack{vert.id, make([]int, n), make([]int, n), make([]bool, n)}
		toSend := false
		for i := 0; i < n; i++ {
			tempPack.valid[i] = vert.R.changed[i]
			if tempPack.valid[i] {
				tempPack.nextHop[i] = vert.R.nextHop[i]
				tempPack.cost[i] = vert.R.cost[i]
				vert.R.changed[i] = false
				toSend = true
			}
		}
		vert.routing_mutex.Unlock()

		if toSend {
			for i := 0; i < len(vert.next); i++ {
				vert.out[i] <- &tempPack
			}
		}
	}
}

func receiver(vert vertice, change chan bool) {
	for {
		tempPack := <-vert.in
		vert.routing_mutex.Lock()
		for i := 0; i < n; i++ {
			if tempPack.valid[i] {
				if tempPack.cost[i]+1 < vert.R.cost[i] {
					vert.R.nextHop[i] = tempPack.senderId
					vert.R.cost[i] = tempPack.cost[i] + 1
					vert.R.changed[i] = true
					change <- true
					fmt.Println("Wierzchołek", vert.id, "aktualizuje ścieżkę do", i, "~  Routing table:", vert.R)
				}
			}
		}
		vert.routing_mutex.Unlock()
	}
}

func forwPass(vert vertice) {
	for {
		if len(*vert.pack_queue) > 0 {
			temp_pack := (*vert.pack_queue)[0]
			vert.queue_mutex.Lock()
			*vert.pack_queue = (*vert.pack_queue)[1:]
			vert.queue_mutex.Unlock()
			temp_pack.visited = append(temp_pack.visited, vert.id)

			recv := temp_pack.receiver
			if recv.router == vert.id {
				vert.hosts[recv.host].in <- &temp_pack
			} else {
				next := vert.R.nextHop[temp_pack.receiver.router]
				for i := 0; i < len(vert.next); i++ {
					if vert.next[i] == next {
						vert.pack_out[i] <- &temp_pack
						break
					}
				}
			}
		} else {
			time.Sleep(time.Millisecond)
		}
	}
}

func forwReceive(vert vertice) {
	for {
		temp_pack := <-vert.pack_in

		vert.queue_mutex.Lock()
		*vert.pack_queue = append(*vert.pack_queue, *temp_pack)
		vert.queue_mutex.Unlock()
	}
}

func hostWork(H host) {
	new_address := address_list[rand.Intn(len(address_list))]
	for {
		if new_address.router != H.router_id || new_address.host != H.id {
			break
		}
		new_address = address_list[rand.Intn(len(address_list))]
	}
	H.out <- &standard_pack{address{H.router_id, H.id}, new_address, make([]int, 0)}

	for {
		temp_pack := <-H.in
		fmt.Print("(", H.router_id, ", ", H.id, ") odebrał paczkę od (", temp_pack.sender.router, ", ", temp_pack.sender.host,
			"), która przebyła ", temp_pack.visited, "\n")
		time.Sleep(time.Millisecond * time.Duration(300))
		H.out <- &standard_pack{address{H.router_id, H.id}, temp_pack.sender, make([]int, 0)}
	}
}

func finish(change chan bool, done chan bool) {
	for {
		select {
		case <-change:
		case <-time.After(time.Millisecond * time.Duration(max_delay)):
			done <- true
			break
		}
	}
}

func main() {
	if len(os.Args) != 4 {
		fmt.Println("go run main.go $n $d $max_hosts_per_router")
		return
	}

	rand.Seed(time.Now().UTC().UnixNano())
	n, _ = strconv.Atoi(os.Args[1])
	d, _ = strconv.Atoi(os.Args[2])
	max_hosts, _ = strconv.Atoi(os.Args[3])

	verts := make([]vertice, n)

	generateGraph(verts)
	printGraph(verts)

	done := make(chan bool)
	change := make(chan bool)

	for i := 0; i < n; i++ {
		for j := 0; j < len(verts[i].hosts); j++ {
			go hostWork(verts[i].hosts[j])
		}
		go sender(verts[i])
		go receiver(verts[i], change)
		go forwPass(verts[i])
		go forwReceive(verts[i])
	}
	go finish(change, done)

	<-done

	fmt.Println("Routing zakończony")

	time.Sleep(time.Second * time.Duration(5))

	fmt.Println("\nTablica routingu:")
	for i := 0; i < n; i++ {
		fmt.Println(verts[i].id, ":")
		for j := 0; j < n; j++ {
			fmt.Print("{nextHop[", j, "] = ", verts[i].R.nextHop[j])
			fmt.Print(" cost[", j, "] = ", verts[i].R.cost[j], "}\n")
		}
		fmt.Println()
	}
	fmt.Println()
}
