package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"
)

var n int              // liczba wierzchołków
var d int              // liczba krawędzi (i,j) gdzie i < j
var b int              // liczba krawędzi (j,i) gdzie i < j
var k int              // liczba pakietów
var h int              // czas życia pakietu
var poacherEnabled int // 1 => kłusownik włączony,  0 => wyłączony
var max_delay = 500    // maksymalne opóźnienie dla sendera, forwardera i receivera
var trap_delay = 2000  // maksymalne opóźnienie dla kłusownika

var print_channel chan string
var thrash_channel chan *pack

type vertice struct {
	id    int
	next  []int
	in    chan *pack
	out   []chan *pack
	trap  chan bool
	packs []int

	temp []int // tablica wykorzystywana jedynie do wygodniejszego tworzenia skrótów postaci (i,j), i>j
}

type pack struct {
	id      int
	ttl     int
	visited []int
}

/*
	Generowanie grafu działa na zasadzie utworzenia połączeń między wierzchołkami 0->1->2->...->n-1,
	a następnie na losowym wybraniu d dodatkowych krawędzi postaci (i,j) oraz b dodatkowych krawędzi postaci (j,i), dla i < j.
	W przypadku zbyt dużego d zostaną wygenerowane wszystkie możliwe skróty postaci (i,j). Analogicznie z b i (j,i).
*/
func generateGraph(verts []vertice, packs []pack, send_channel chan *pack, receive_channel chan *pack) {
	mutual := make(chan *pack)
	for i := 0; i < n; i++ {
		verts[i].id = i
		verts[i].next = make([]int, 0)
		verts[i].temp = make([]int, 0)
		verts[i].in = mutual
		verts[i].trap = make(chan bool)
		mutual = make(chan *pack)
		verts[i].out = make([]chan *pack, 0)
		if i != n-1 {
			verts[i].next = append(verts[i].next, i+1)
			verts[i].out = append(verts[i].out, mutual)
		}
	}
	verts[0].in = send_channel
	verts[n-1].out = append(verts[n-1].out, receive_channel)
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
	i = 0
	for i < b {
		full := true
		for j := 0; j < n; j++ {
			if len(verts[j].temp) != j {
				full = false
				break
			}
		}
		if full {
			break
		}

		exists := false
		index := 1 + rand.Intn(n-1)
		new_next := rand.Intn(index)

		for _, value := range verts[index].temp {
			if value == new_next {
				exists = true
				break
			}
		}
		if !exists {
			verts[index].temp = append(verts[index].temp, new_next)
			verts[index].out = append(verts[index].out, verts[new_next].in)
			i++
		}
	}
	for i := 0; i < n; i++ {
		verts[i].next = append(verts[i].next, verts[i].temp...)
	}
	for i := 0; i < k; i++ {
		packs[i] = pack{i, h, make([]int, 0)}
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
func starter(packs []pack, send_channel chan *pack) {
	for i := 0; i < k; i++ {
		print_channel <- "\tPakiet " + strconv.Itoa(packs[i].id) + " jest wysyłany"
		time.Sleep(time.Millisecond * time.Duration(rand.Intn(max_delay)))
		send_channel <- &packs[i]
	}
}

/*
	Funkcja forwardera (wierzchołka), który w danym momencie przyjmuje pakiet lub pułapkę na pakiet,
	a po odczekaniu [0,max_delay) milisekund próbuje przesłać pakiet dalej.
	W przypadku natrafienia na pułapkę lub przekroczenia TTL pakiet przepada.
*/
func forwarder(vert *vertice) {
	trap_counter := 0
	for {
		select {
		case data := <-vert.in:
			(*vert).packs = append((*vert).packs, data.id)
			data.visited = append(data.visited, (*vert).id)

			print_channel <- "\tPakiet " + strconv.Itoa(data.id) + " jest w wierzchołku " + strconv.Itoa((*vert).id)
			time.Sleep(time.Millisecond * time.Duration(rand.Intn(max_delay)))

			if trap_counter > 0 {
				print_channel <- "\tPakiet " + strconv.Itoa(data.id) + " wpadł w pułapkę w wierzchołku " + strconv.Itoa((*vert).id)
				thrash_channel <- data
				trap_counter--
			} else if data.ttl > 0 {
				data.ttl--
				passToNext((*vert).out, data)
			} else {
				print_channel <- "\tPakiet " + strconv.Itoa(data.id) + " - śmierć w wierzchołku " + strconv.Itoa((*vert).id)
				thrash_channel <- data
			}

		case <-vert.trap:
			trap_counter++
		}
	}
}

/*
	Funkcja przesłania pakietu, która losuje potencjalnego odbiorcę i próbuje wysłać do niego pakiet.
	W przypadku gdy nie jest to możliwe, proces jest powtarzany rekurencyjnie aż do skutku.
*/
func passToNext(out []chan *pack, item *pack) {
	next := rand.Intn(len((out)))
	select {
	case out[next] <- item:

	default:
		passToNext(out, item)
	}
}

/*
	Funkcja odbiorcy, który co [0,max_delay) milisekund próbuje odebrać pakiet z ujścia.
*/
func receiver(receive_channel chan *pack) {
	for data := range receive_channel {
		print_channel <- "\tPakiet " + strconv.Itoa(data.id) + " jest odebrany"
		thrash_channel <- data
		time.Sleep(time.Millisecond * time.Duration(rand.Intn(max_delay)))
	}
}

/*
	Funkcja kłusownika, który co pewien czas wysyła pułapkę do losowego wierzchołka
*/
func poacher(verts []vertice) {
	for {
		time.Sleep(time.Millisecond * time.Duration(rand.Intn(trap_delay)))
		next := rand.Intn(len((verts)))
		verts[next].trap <- true
		print_channel <- fmt.Sprint("\t\tWysłano pułapkę do ", next)
	}
}

/*
	Funkcja, która zbiera zużyte pakiety.
*/
func thrash(wg *sync.WaitGroup) {
	for range thrash_channel {
		wg.Done()
	}
}

/*
	Funkcja usuwająca powtórzenia w tablicy (slice)
*/
func removeDuplicateValues(intSlice []int) []int {
	keys := make(map[int]bool)
	list := []int{}

	for _, entry := range intSlice {
		if _, value := keys[entry]; !value {
			keys[entry] = true
			list = append(list, entry)
		}
	}
	return list
}

func main() {
	if len(os.Args) != 7 {
		fmt.Println("go run main.go $n $d $b $k $h $kłusownik(0 => wyłączony, 1 => włączony)")
		return
	}

	rand.Seed(time.Now().UTC().UnixNano())
	n, _ = strconv.Atoi(os.Args[1])
	d, _ = strconv.Atoi(os.Args[2])
	b, _ = strconv.Atoi(os.Args[3])
	k, _ = strconv.Atoi(os.Args[4])
	h, _ = strconv.Atoi(os.Args[5])
	poacherEnabled, _ = strconv.Atoi(os.Args[6])

	verts := make([]vertice, n)
	packs := make([]pack, k)

	send_channel := make(chan *pack)
	receive_channel := make(chan *pack)
	print_channel = make(chan string, 50)
	thrash_channel = make(chan *pack)
	done := make(chan bool)

	go printer(print_channel, done)

	generateGraph(verts, packs, send_channel, receive_channel)
	printGraph(verts)

	var wg sync.WaitGroup
	wg.Add(k)

	go thrash(&wg)
	print_channel <- "Przebieg pakietów:"
	go starter(packs, send_channel)
	for i := 0; i < n; i++ {
		go forwarder(&verts[i])
	}
	go receiver(receive_channel)
	if poacherEnabled != 0 {
		go poacher(verts)
	}

	wg.Wait()

	print_channel <- "\nPakiety:"
	for i := 0; i < k; i++ {
		print_channel <- fmt.Sprint("\t", packs[i].id, " odwiedził ", removeDuplicateValues(packs[i].visited))
	}

	print_channel <- "\nWierzchołki:"
	for i := 0; i < n; i++ {
		print_channel <- fmt.Sprint("\t", verts[i].id, " przepuścił ", removeDuplicateValues(verts[i].packs))
	}
	close(print_channel)
	<-done
}
