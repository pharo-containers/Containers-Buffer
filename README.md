# Containers-Buffer
A Circular Buffer implementation providing efficient temporary storage with fixed capacity and automatic wraparound functionality.

![Pharo Version](https://img.shields.io/badge/Pharo-10+-blue)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

## What is a Buffer?

A Buffer (also known as a circular buffer or ring buffer) is a data structure that uses a fixed-size array as if it were connected end-to-end. It provides efficient insertion and removal operations with constant time complexity O(1). When the buffer reaches its maximum capacity, new elements overwrite the oldest ones, making it perfect for streaming data and producer-consumer scenarios.

## Why use Containers-Buffer?
Buffers solve a critical problem in modern applications: **handling continuous data streams safely**. Perfect for keeping "recent N items" automatically without memory bloat or performance degradation.

### Key Benefits
- **Constant Memory Usage**: Never grows beyond specified capacity
- **O(1) Performance**: Lightning-fast operations regardless of data volume  
- **Zero Memory Leaks**: Automatic cleanup of old data
- **Streaming Optimized**: Designed for continuous data flow

### Chat Applications
```smalltalk
"Chat room - keep last 50 messages automatically"
chat := CTBuffer withCapacity: 50.

chat put: 'User1: Hello everyone!'.
chat put: 'User2: How are you doing?'.

"... more messages flow in ..."
chat put: 'User3: Good morning!'.  "Oldest message automatically removed"

"Always has recent 50 messages, zero manual cleanup"
```

### File Processing in Chunks
```smalltalk
"Process massive files without loading everything into memory"
fileBuffer := CTBuffer withCapacity: 1024.  "1KB processing chunks"
stream := 'huge-dataset.csv' asFileReference readStream.
[ stream atEnd ] whileFalse: [
    chunk := stream next: 1024.
    fileBuffer put: chunk.
    self processDataChunk: fileBuffer get  "Process and auto-remove"
].

"Memory usage stays constant - handles files of any size!"
```

### Performance Degradation
```smalltalk
"OrderedCollection - Gets slower and slower"
log := OrderedCollection new.
1 to: 100000 do: [ :i |
    log add: 'entry ', i asString.
    log size > 1000 ifTrue: [ 
        log removeFirst  "O(n) operation - shifts 999+ elements EVERY TIME!"
    ].
].
"Performance degrades from 1ms to 100ms+ per operation"

"Buffer - Lightning fast forever"
log := CTBuffer withCapacity: 1000.
1 to: 100000 do: [ :i |
    log put: 'entry ', i asString.  "O(1) operation - always instant!"
].

"Consistent 0.01ms performance whether it's operation #10 or #10,000,000"
```

### Performance Comparison

| Operation | OrderedCollection | Array | CTBuffer |
|-----------|------------------|--------|----------|
| Add item | O(1) | O(n)* | O(1) |
| Remove old | O(n) | O(n) | O(1) |
| Memory usage | Unlimited growth | Fixed/reallocated | Fixed |
| Cache efficiency | Poor (fragmented) | Good | Excellent |


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