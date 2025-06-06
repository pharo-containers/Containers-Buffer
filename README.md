# Containers-Buffer
A Circular Buffer implementation providing efficient temporary storage with fixed capacity and automatic wraparound functionality.

![Pharo Version](https://img.shields.io/badge/Pharo-10+-blue)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

## What is a Buffer?

A Buffer (also known as a circular buffer or ring buffer) is a data structure that uses a fixed-size array as if it were connected end-to-end. It provides efficient insertion and removal operations with constant time complexity O(1). When the buffer reaches its maximum capacity, new elements overwrite the oldest ones, making it perfect for streaming data and producer-consumer scenarios.

## Loading 
The following script installs Containers-Buffer in Pharo.

```smalltalk
Metacello new
  baseline: 'ContainersBuffer';
  repository: 'github://pharo-containers/Containers-Buffer/src';
  load.
```

## If you want to depend on it 

Add the following code to your Metacello baseline or configuration 

```smalltalk
spec 
   baseline: 'ContainersBuffer' 
   with: [ spec repository: 'github://pharo-containers/Containers-Buffer/src' ].
```

## Contributing

This is part of the Pharo Containers project. Feel free to contribute by implementing additional methods, improving tests, or enhancing documentation.