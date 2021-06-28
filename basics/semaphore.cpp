//https://stackoverflow.com/questions/4792449/c0x-has-no-semaphores-how-to-synchronize-threads
//https://en.cppreference.com/w/cpp/thread/counting_semaphore

#include <iostream>
#include <thread>
#include <chrono>
#include <mutex>
#include <condition_variable>

class semaphore {
    std::mutex mutex;
    std::condition_variable condition;
    unsigned long count = 0;

public:
    void release() {
        //std::lock_guard<decltype(mutex)> lock(mutex);
        mutex.lock();
		++count;
        condition.notify_one();
		mutex.unlock();
    }

    void acquire() {
        std::unique_lock<decltype(mutex)> lock(mutex);
		while(!count)
            condition.wait(lock);
        --count;
    }

    bool try_acquire() {
		std::lock_guard<decltype(mutex)> lock(mutex);
        if(count) {
            --count;
            return true;
        }
        return false;
    }
};

semaphore sem1;
semaphore sem2;
 
void threadFunc()
{	
	sem1.acquire();
 
	std::cout << "[thread] Got the signal\n";
	std::this_thread::sleep_for(std::chrono::seconds(1));
	std::cout << "[thread] Send the signal\n";
 
	sem2.release();
}
 
int main()
{
	std::thread myThread(threadFunc);
 
	std::cout << "[main] Send the signal\n";
 
	sem1.release();
 
	sem2.acquire();
 
	std::cout << "[main] Got the signal\n";
	myThread.join();
}