"
I am an abstract circular buffer with fixed capacity. I provide common functionality for both FIFO and LIFO circular buffers. My subclasses implement specific ordering behaviors by overriding updateReadIndex.
"
Class {
	#name : 'CTAbstractBuffer',
	#superclass : 'Object',
	#instVars : [
		'elements',
		'readIndex',
		'writeIndex',
		'currentSize',
		'capacity'
	],
	#category : 'Containers-Buffer',
	#package : 'Containers-Buffer'
}

{ #category : 'instance creation' }
CTAbstractBuffer class >> withCapacity: anInteger [

	anInteger < 1 ifTrue: [ self error: 'Capacity must be positive' ].
	^ self new
		capacity: anInteger;
		yourself
]

{ #category : 'accessing' }
CTAbstractBuffer >> availableSpace [

	"Return the number of additional elements that can be stored"
	
	^ capacity - currentSize
]

{ #category : 'accessing' }
CTAbstractBuffer >> capacity [

	"Return the maximum capacity of the buffer"
	
	^ capacity
]

{ #category : 'accessing' }
CTAbstractBuffer >> capacity: anInteger [

		"Set the capacity and initialize elements array"
	
	capacity := anInteger.
	elements := Array new: capacity
]

{ #category : 'accessing' }
CTAbstractBuffer >> elements [

	^ elements
]

{ #category : 'initialization' }
CTAbstractBuffer >> initialize [

	super initialize.
	capacity := 10.
	elements := Array new: capacity.
	readIndex := 1.
	writeIndex := 1.
	currentSize := 0
]

{ #category : 'testing' }
CTAbstractBuffer >> isEmpty [
	
	"Return true if buffer has no elements"
	
	^ currentSize = 0
]

{ #category : 'testing' }
CTAbstractBuffer >> isFull [

		"Return true if buffer is at maximum capacity"
	
	^ currentSize = capacity

]

{ #category : 'accessing' }
CTAbstractBuffer >> readIndex [

	"Return the current read index"
	
	^ readIndex

]

{ #category : 'accessing' }
CTAbstractBuffer >> size [

	"Return the current number of elements in the buffer"
	
	^ currentSize
]

{ #category : 'accessing' }
CTAbstractBuffer >> writeIndex [

	"Return the current write index"
	
	^ writeIndex
]
