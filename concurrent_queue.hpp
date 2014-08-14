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

	public:

		T pop()
		{
			std::unique_lock<std::mutex> locker(_mutex);
			while (_queue.empty()) _condition.wait(locker);
			auto t = _queue.front();
			_queue.pop();
			return t;
		}

		void push(const T & t)
		{
			std::unique_lock<std::mutex> locker(_mutex);
			_queue.push(t);
			locker.unlock();
			_condition.notify_one();
		}

		void push(T && t)
		{
			std::unique_lock<std::mutex> locker(_mutex);
			_queue.push(std::move(t));
			locker.unlock();
			_condition.notify_one();
		}

		template <typename InputIterator>
		void push(InputIterator begin, InputIterator end)
		{
			std::unique_lock<std::mutex> locker(_mutex);
			for (auto iter = begin; iter != end; ++iter)
				_queue.push_back(std::move(*iter));
			locker.unlock();
			_condition.notify_all();
		}

		concurrent_queue() = default;
		concurrent_queue(Queue _queue) : _queue(std::move(_queue)) {}
		concurrent_queue(const concurrent_queue &) = delete;
		concurrent_queue& operator = (const concurrent_queue &) = delete;

};



#endif

