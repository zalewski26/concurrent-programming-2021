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
var k int
var max_delay int

type vertice struct {
	id       int
	next     []int
	in       chan *pack
	out      []chan *pack
	packages []int
}

type pack struct {
	id      int
	visited []int
}

/*
	Generowanie grafu działa na zasadzie utworzenia połączeń między wierzchołkami 0->1->2->...->n-1,
	a następnie na losowym wybraniu d dodatkowych połączeń (jeśli d jest zbyt duże, zostaną dodane wszystkie możliwe skróty).
*/
func generateGraph(verts []vertice, packs []pack) {
	mutual := make(chan *pack)
	for i := 0; i < n; i++ {
		verts[i].id = i
		verts[i].next = make([]int, 0)
		verts[i].in = mutual
		mutual = make(chan *pack)
		verts[i].out = make([]chan *pack, 0)
		if i != n-1 {
			verts[i].next = append(verts[i].next, i+1)
			verts[i].out = append(verts[i].out, mutual)
		}
	}
	i := 0
	for i < d {
		full := true
		for j := 0; j < n; j++ {
			if len(verts[j].next) != n-1-j {
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
			i++
		}
	}
	for i := 0; i < k; i++ {
		packs[i] = pack{i, make([]int, 0)}
	}
}

/*
	Drukowanie grafu
*/
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

/*
	Funkcja wykorzystana do synchronizacji drukowania komunikatów
*/
func printer(in <-chan string, done chan<- bool) {
	for txt := range in {
		fmt.Println(txt)
	}
	done <- true
}

/*
	Funkcja wysyłającego, który w odstępach [0,max_delay) milisekund wysyła nowy pakiet do źródła.
*/
func starter(packs []pack, send_channel chan *pack, print_channel chan string) {
	for i := 0; i < k; i++ {
		print_channel <- "\tPakiet " + strconv.Itoa(packs[i].id) + " jest wysyłany"
		time.Sleep(time.Millisecond * time.Duration(rand.Intn(max_delay)))
		send_channel <- &packs[i]
	}
}

/*
	Funkcja forwardera (wierzchołka), który w danym momencie przyjmuje jeden pakiet, a po odczekaniu [0,max_delay) milisekund przesyła go dalej.
*/
func forwarder(vert *vertice, print_channel chan string) {
	for data := range vert.in {
		(*vert).packages = append((*vert).packages, data.id)
		data.visited = append(data.visited, (*vert).id)

		print_channel <- "\tPakiet " + strconv.Itoa(data.id) + " jest w wierzchołku " + strconv.Itoa((*vert).id)
		time.Sleep(time.Millisecond * time.Duration(rand.Intn(max_delay)))
		(*vert).out[rand.Intn(len((*vert).out))] <- data
	}
}

/*
	Funkcja odbiorcy, który co [0,max_delay) milisekund próbuje odebrać pakiet ze źrodła.
*/
func receiver(receive_channel chan *pack, wg *sync.WaitGroup, print_channel chan string) {
	for data := range receive_channel {
		print_channel <- "\tPakiet " + strconv.Itoa(data.id) + " jest odebrany"
		wg.Done()
		time.Sleep(time.Millisecond * time.Duration(rand.Intn(max_delay)))
	}
}

func main() {
	if len(os.Args) != 5 {
		fmt.Println("go run main.go $liczbaWierzchołków $liczbaSkrótów $liczbaPakietów $maxOpóźnienieMs")
		return
	}

	rand.Seed(time.Now().UTC().UnixNano())
	n, _ = strconv.Atoi(os.Args[1])
	d, _ = strconv.Atoi(os.Args[2])
	k, _ = strconv.Atoi(os.Args[3])
	max_delay, _ = strconv.Atoi(os.Args[4])

	vertices := make([]vertice, n)
	packages := make([]pack, k)

	print_channel := make(chan string, 50)
	send_channel := make(chan *pack)
	receive_channel := make(chan *pack)
	done := make(chan bool)

	go printer(print_channel, done)

	generateGraph(vertices, packages)
	printGraph(vertices)

	var wg sync.WaitGroup
	wg.Add(k)

	print_channel <- "Przebieg pakietów:"
	vertices[0].in = send_channel
	vertices[n-1].out = append(vertices[n-1].out, receive_channel)
	go starter(packages, send_channel, print_channel)
	for i := 0; i < n; i++ {
		go forwarder(&vertices[i], print_channel)
	}
	go receiver(receive_channel, &wg, print_channel)
	wg.Wait()

	print_channel <- "\nPakiety:"
	for i := 0; i < k; i++ {
		msg := fmt.Sprint("\t", packages[i].id, " odwiedził ", packages[i].visited)
		print_channel <- msg
	}

	print_channel <- "\nWierzchołki:"
	for i := 0; i < n; i++ {
		msg := fmt.Sprint("\t", vertices[i].id, " przepuścił ", vertices[i].packages)
		print_channel <- msg
	}
	close(print_channel)
	<-done
}
