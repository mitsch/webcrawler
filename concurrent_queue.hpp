/// @file concurrrent_queue.hpp
/// @author Michael Koch

#ifndef __CONCURRENT_QUEUE_HPP__
#define __CONCURRENT_QUEUE_HPP__

#include <queue>
#include <mutex>
#include <thread>
#include <condition_variable>
#include <algorithm>

template <typename T, typename Queue = std::queue<T>>
class concurrent_queue
{
	
	private:

		Queue _queue;
		std::mutex _mutex;
		std::condition_variable _condition;
		bool _closed = false;

	public:

		void close()
		{
			std::unique_lock<std::mutex> locker(_mutex);
			_closed = true;
		}
		
		bool try_pop (T & t)
		{
			bool success;
			std::unique_lock<std::mutex> locker(_mutex);
			while (not _closed && _queue.empty()) _condition.wait(locker);
			if (not _queue.empty()) success = true, t = _queue.front(), _queue.pop();			
			else success = false;
			return success;
		}

		void push(const T & t)
		{
			std::unique_lock<std::mutex> locker(_mutex);
			_queue.push(t);
			_closed = false;
			locker.unlock();
			_condition.notify_all();
		}

		void push(T && t)
		{
			std::unique_lock<std::mutex> locker(_mutex);
			_queue.push(std::move(t));
			_closed = false;
			locker.unlock();
			_condition.notify_all();
		}

		template <typename InputIterator>
		void push(InputIterator begin, InputIterator end)
		{
			std::unique_lock<std::mutex> locker(_mutex);			
			for (auto iter = begin; iter != end; ++iter)
				_queue.push_back(std::move(*iter));
			_closed = false;
			locker.unlock();
			_condition.notify_all();
		}

		concurrent_queue() = default;
		concurrent_queue(Queue _queue) : _queue(std::move(_queue)) {}
		concurrent_queue(const concurrent_queue &) = delete;
		concurrent_queue& operator = (const concurrent_queue &) = delete;

};



#endif

