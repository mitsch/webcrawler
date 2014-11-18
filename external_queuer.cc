/// @file external_queuer.cc
/// @author Michael Koch
/// @copyright CC BY 3.0

#include <iostream>
#include <string>
#include <queue>
#include <thread>
#include <atomic>
#include "concurrent_queue.hpp"

int main (const int argc, const char ** argv)
{
	concurrent_queue<std::string> seeds;

	auto reader = std::thread([&seeds]()
	{
		std::string seed;
		
		while (std::cin)
		{
			std::getline(std::cin, seed);
			if (not seed.empty())
				seeds.push(std::move(seed));
		}
		
		seeds.close();
	});
	
	auto writer = std::thread([&seeds]()
	{
		std::string seed;
				
		while (seeds.try_pop(seed) && std::cout)
		{
			std::cout << seed << std::endl;
		}
	});

	reader.join();
	writer.join();

	return 0;
}

