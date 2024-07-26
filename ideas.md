# ITU links

[ITU Research Project and Thesis](https://wiki.itu.dk/computerscience/index.php/Research_Project_and_Thesis)

# Christian Ideas

## Scalable distributed financial transaction system

Could be a proof of concept using [Tigerbeetle](https://tigerbeetle.com). See [demo here](https://www.youtube.com/watch?v=sC1B3d9C_sI)

Further reading: 

- [Scalable OLTP in the Cloud: What’s the BIG DEAL?](https://www.cidrdb.org/cidr2024/papers/p63-helland.pdf)

- [Evolution of Financial Exchange Architectures](https://www.infoq.com/presentations/financial-exchange-architecture/)

## Implementing some kind of algo as fast as possible

- Pick a baseline algorithm implementation and API

- Benchmark its performance, analyse the results, create hypothesis about where improvements could be found using theory from functioncal programming, stack/heap etc, profile the memory usages/allocations etc

- Research and document optimizations, create new iteration that implement optimizations, benchmark it again and compare performance to prior iterations in a systematic fashion

Inspiration: [The Fastest .NET Dictionary](https://youtu.be/le_1yMroz80?si=hdavMNVBmI8q2OwW). Repo [here](https://github.com/matthewcrews/FastDictionaryTest)

## Something about using [DMI's REST API](https://opendatadocs.dmi.govcloud.dk/en/Basics) to fetch metrological data

- Use DMI's api to get data
- Build some kind of app with the data

# Mathias ideas

## Functional programming for web development:

Compare functional languages for web development
Pros/cons of using functional languages for web instead of javascript etc.
Build a web app in a functional language. 

## Using functional and imperative programming:

Could be a comparison of the two.
Could also look into how functional programming integrates into imperative programming (or object oriented programming).
Build some kind of project integrating the two paradigms. 

## Use Functional Reactive Programming (FRP) for real-time data visualization:

(I haven't read much about this topic, but skatten gives an explanation that sounds quite interesting):
Functional Reactive Programming (FRP) is a programming paradigm for working with reactive systems in a declarative way. 
It combines the principles of functional programming with reactive programming, allowing developers to model dynamic systems that change over time. 
FRP is particularly useful for applications that require continuous updates and real-time responsiveness, such as user interfaces, animations, and interactive simulations.

## Build a "bot"/app, for automatically updating node packages. (Just brainstorming everything at this point :D) 

Not sure how possible this is, but it's just a thought. 
At TV2 we use expensive developers time to go through all our repos, and update package versions when they are outdated or newer versions are available.
I believe that it should be possible to create some kind of app or bot to do this automatically.
Idea is to have the bot look through all packages and find those that have newer version. Then create unit tests for all code in repo (maybe ai generated??) to make sure all functionality works as intended after upgrading version. Bot then automatically updates version, if all tests pass. 
It kind of exists [Renovate](https://docs.renovatebot.com/reading-list) but not ai powered

## In addition to your weather app idea:
Setup everything for the weather app: build the app, create pipelines (CI/CD), create dev/prod environments, test the app for some users (maybe both some developers for backend part and regular people for frontend part)
The addition: I think that it would be awesome to be able to draw a route on a map (it could be your route for a long bicycle ride), and then be able to see the weather forecast for the entire route, calculated by taking the forecast for a city at the time you expect to be near that city on your route. (This could be our "innovative", "new", "special", "amazing" way of making the project thesis/research project "friendly")

### Development/production environments (no matter which project we create)

It would be super cool to set up dev and prod environments for our project. 
Then create pipeline (github actions) to build our changes on the dev env, so we can test it there before building prod. 
I think this would be a less important part of the project, but still a super cool flex, showing that we indeed are super cool :D


# ChatGPT ideas

Generated with ChatGPT. [Link to chat](https://chatgpt.com/share/0b4c8d35-bb0e-4851-81d1-3fedabc3d337)

1. **Parallelized Graph Algorithms for Social Network Analysis**: Develop parallelized graph algorithms using functional concurrent programming for analyzing social networks in real-time via a web API.

2. **Distributed Machine Learning Framework with Asynchronous Updates**: Design a distributed machine learning framework with asynchronous updates for training models on large datasets accessible via a web API.

3. **Concurrent Data Processing Pipeline for Big Data Analytics**: Create a concurrent data processing pipeline for big data analytics, enabling real-time data analysis through a web API.

4. **Parallel Genetic Algorithms for Optimization Problems**: Implement parallel genetic algorithms for optimization problems and provide optimization services via a web API.

5. **Concurrent Blockchain Protocol Implementation**: Design and implement a concurrent blockchain protocol with a web API for interacting with the blockchain network.

6. **Real-time Image Processing and Analysis Pipeline**: Develop a concurrent image processing and analysis pipeline accessible via a web API for real-time image analysis.

7. **Concurrency in Natural Language Processing (NLP)**: Explore concurrent techniques for NLP tasks and provide text analysis services via a web API.

8. **Parallelized Evolutionary Algorithms for Multi-objective Optimization**: Develop parallel evolutionary algorithms for multi-objective optimization accessible through a web API.

9. **Concurrent Data Structures for High-Performance Computing**: Research and implement concurrent data structures optimized for high-performance computing, accessible via a web API.

10. **Functional Reactive Programming for Real-time Web Applications**: Utilize functional reactive programming for building real-time web applications with interactive features via a web API.

11. **Parallel Simulation and Modeling for Scientific Computing**: Develop parallel simulation and modeling techniques for scientific computing accessible via a web API.

12. **Concurrency in Financial Trading Systems**: Investigate concurrent programming techniques for financial trading systems, offering algorithmic trading services via a web API.

## Some more chat ideas:
##Scalable Web Application Architectures
Explore different architectures for building scalable web applications, focusing on the use of microservices, serverless computing, and containerization. Evaluate the performance, scalability, and maintainability of each approach.

Possible Sub-topics:
Microservices vs. Monolithic Architectures: Performance and scalability comparison.
Serverless Architectures: Benefits and challenges in web development.
Container Orchestration with Kubernetes: Implementing and managing scalable web applications.

##Real-time Web Applications
Description:
Explore the technologies and frameworks used to build real-time web applications, such as WebSockets, Server-Sent Events (SSE), and WebRTC. Focus on use cases like live data streaming, online gaming, and collaborative tools.

Possible Sub-topics:
WebSockets vs. SSE: Performance and use case analysis.
Implementing Real-time Collaboration Tools: Building a collaborative document editor or a multiplayer game.
WebRTC for Peer-to-Peer Communication: Applications in video conferencing and file sharing.
