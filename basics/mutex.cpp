//https://www.cplusplus.com/reference/mutex/mutex/

#include <iostream>    
#include <thread>         
#include <mutex>

std::mutex mtx;

void print_block (int n, char c) {
  	mtx.lock();
  	for (int i = 0; i < n; i++) { 
		  std::cout << c; 
		  std::this_thread::sleep_for(std::chrono::milliseconds(1));
	}
  	std::cout << '\n';
  	mtx.unlock();
}

int main ()
{
  	std::thread th1 (print_block,50,'*');
  	std::thread th2 (print_block,50,'$');

  	th1.join();
  	th2.join();

  	return 0;
}