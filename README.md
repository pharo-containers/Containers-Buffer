# Containers-Buffer
A Circular Buffer implementation providing efficient temporary storage with fixed capacity and automatic wraparound functionality. Available in both FIFO (First In, First Out) and LIFO (Last In, First Out) variants.

![Pharo Version](https://img.shields.io/badge/Pharo-10+-blue)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

## What is a Buffer?

A Buffer (also known as a circular buffer or ring buffer) is a data structure that uses a fixed-size array as if it were connected end-to-end. It provides efficient insertion and removal operations with constant time complexity O(1). When the buffer reaches its maximum capacity, new elements overwrite the oldest ones, making it perfect for streaming data and producer-consumer scenarios.

### We provide two types of buffers:
- FIFO Buffer - First In, First Out (Queue behavior)
- LIFO Buffer - Last In, First Out (Stack behavior)

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

## Why use Containers-Buffer?
Buffers solve a critical problem in modern applications: **handling continuous data streams safely**. Perfect for keeping "recent N items" automatically without memory bloat or performance degradation.

### Key Benefits
- **Constant Memory Usage**: Never grows beyond specified capacity
- **O(1) Performance**: Lightning-fast operations regardless of data volume  
- **Zero Memory Leaks**: Automatic cleanup of old data
- **Streaming Optimized**: Designed for continuous data flow
- **Flexible Ordering**: Choose FIFO or LIFO based on your needs


## FIFO Buffer Use Cases

### Chat Applications
This example demonstrates how to use a FIFO buffer to maintain a chat history, automatically removing the oldest messages when new ones are added.

```smalltalk
"Chat room - keep last 50 messages automatically"
chat := CTFIFOBuffer withCapacity: 50.

chat push: 'User1: Hello everyone!'.
chat push: 'User2: How are you doing?'.

"... more messages flow in ..."
chat push: 'User3: Good morning!'.  "Oldest message automatically removed"

"Always has recent 50 messages, zero manual cleanup"
```

## LIFO Buffer Use Cases

### Undo/Redo Functionality
This example shows how to implement undo/redo functionality in an editor using a LIFO buffer, allowing users to revert their last actions easily.

```smalltalk
"Implementing undo/redo in an editor"
undoBuffer := CTLIFOBuffer withCapacity: 20.  "Keep last 20 actions"
undoBuffer push: 'Hello World'.
undoBuffer push: 'Add bold formatting'.
undoBuffer push: 'Add italic formatting'.

"User presses undo"
lastAction := undoBuffer pop.  "Returns 'Add italic formatting' and removes it"

"User can redo by pushing it back"
undoBuffer push: lastAction.  "Puts 'Add italic formatting' back on top"
"Now 'Add italic formatting' is on top, ready to be undone next"
```

### Browser History
This example demonstrates how to implement a simple browser history using a LIFO buffer, allowing users to navigate back through their most recent pages.

```smalltalk
"Browser back button - show most recent pages first"
browserHistory := CTLIFOBuffer withCapacity: 20.

browserHistory push: 'https://pharo.org'.
browserHistory push: 'https://github.com/pharo-containers'.
browserHistory push: 'https://stackoverflow.com/questions/...'.

"Back button gets most recent page"
previousPage := browserHistory pop.  "Gets 'https://stackoverflow.com/questions/...' (most recent)"
previousPage := browserHistory pop.  "Gets 'https://github.com/pharo-containers' (second most recent)"
previousPage := browserHistory pop.  "Gets 'https://pharo.org' (third most recent)"
```


### Performance Comparison
```smalltalk
"OrderedCollection - Gets slower and slower"
log := OrderedCollection new.
1 to: 100000 do: [ :i |
    log add: 'entry ', i asString.
    log size > 1000 ifTrue: [ 
        log removeFirst  "O(n) operation - shifts 999+ elements EVERY TIME!"
    ].
].

"Buffer - Fast and automatic"
log := CTFIFOBuffer withCapacity: 1000.
1 to: 100000 do: [ :i |
    log push: 'entry ', i asString.  "O(1) operation - Automatic Management !"
].
```
### Comparison
| Operation | OrderedCollection | Array | CTFIFOBuffer |
|-----------|------------------|--------|-------------|
| Add item | O(1) | O(1) | O(1) |
| Remove old | O(n) | O(n) | O(1) |
| Memory usage | Unlimited growth | Fixed/reallocated | Fixed |


## Contributing

This is part of the Pharo Containers project. Feel free to contribute by implementing additional methods, improving tests, or enhancing documentation.