package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"
)

var n int
var d int
var max_delay = 500

type vertice struct {
	sync.Mutex
	id   int
	next []int
	in   chan *pack
	out  []chan *pack
	R    routingTable
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

func generateGraph(verts []vertice) {
	mutual := make(chan *pack)
	for i := 0; i < n; i++ {
		verts[i].id = i
		verts[i].next = make([]int, 0)
		verts[i].in = mutual
		mutual = make(chan *pack)
		verts[i].out = make([]chan *pack, 0)

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

		}
		if i != 0 {
			verts[i].next = append(verts[i].next, i-1)
			verts[i].out = append(verts[i].out, verts[i-1].in)
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
			verts[index].R.nextHop[new_next] = new_next
			verts[index].R.cost[new_next] = 1

			verts[new_next].next = append(verts[new_next].next, index)
			verts[new_next].out = append(verts[new_next].out, verts[index].in)
			verts[new_next].R.nextHop[index] = index
			verts[new_next].R.cost[index] = 1
			i++
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
		fmt.Println("]")
	}
}

func sender(vert vertice) {
	for {
		time.Sleep(time.Millisecond * time.Duration(rand.Intn(max_delay)))
		vert.Lock()
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
		vert.Unlock()

		if toSend {
			for i := 0; i < len(vert.next); i++ {
				fmt.Println("Wierzchołek", vert.id, "wysyła paczkę:", tempPack, "do", vert.next[i])
				vert.out[i] <- &tempPack
			}
		}
	}
}

func receiver(vert vertice, change chan bool) {
	for {
		tempPack := <-vert.in
		vert.Lock()
		for i := 0; i < n; i++ {
			if tempPack.valid[i] {
				if tempPack.cost[i]+1 < vert.R.cost[i] {
					vert.R.nextHop[i] = tempPack.senderId
					vert.R.cost[i] = tempPack.cost[i] + 1
					vert.R.changed[i] = true
					change <- true
					fmt.Println("Wierzchołek", vert.id, "aktualizuje ścieżkę do", i)
				}
			}
		}
		vert.Unlock()
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
	if len(os.Args) != 3 {
		fmt.Println("go run main.go $n $d")
		return
	}

	rand.Seed(time.Now().UTC().UnixNano())
	n, _ = strconv.Atoi(os.Args[1])
	d, _ = strconv.Atoi(os.Args[2])

	verts := make([]vertice, n)

	generateGraph(verts)
	printGraph(verts)

	fmt.Println("\nWierzchołki:")
	for i := 0; i < n; i++ {
		fmt.Println(verts[i].id, ":")
		for j := 0; j < n; j++ {
			fmt.Print("{nextHop[", j, "] = ", verts[i].R.nextHop[j])
			fmt.Print(" cost[", j, "] = ", verts[i].R.cost[j], "}\n")
		}
		fmt.Println()
	}
	fmt.Println()

	done := make(chan bool)
	change := make(chan bool)

	for i := 0; i < n; i++ {
		go sender(verts[i])
		go receiver(verts[i], change)
	}
	go finish(change, done)

	<-done

	fmt.Println("\nWierzchołki:")
	for i := 0; i < n; i++ {
		fmt.Println(verts[i].id, ":")
		for j := 0; j < n; j++ {
			fmt.Print("{nextHop[", j, "] = ", verts[i].R.nextHop[j])
			fmt.Print(" cost[", j, "] = ", verts[i].R.cost[j], "}\n")
		}
		fmt.Println()
	}
}
