"
I represent a circular buffer with fixed capacity.

A Buffer provides efficient temporary storage with automatic wraparound when full. It supports O(1) insertion and removal operations, making it ideal for streaming data, producer-consumer scenarios, and situations requiring fixed-memory buffering.
"
Class {
	#name : 'CTBuffer',
	#superclass : 'Object',
	#instVars : [
		'elements',
		'maxCapacity',
		'readIndex',
		'writeIndex',
		'currentSize'
	],
	#category : 'Containers-Buffer',
	#package : 'Containers-Buffer'
}

{ #category : 'accessing' }
CTBuffer >> availableSpace [
		"Return the number of additional elements that can be stored"
	
	^ maxCapacity - currentSize
]

{ #category : 'accessing' }
CTBuffer >> capacity [

	"Return the maximum capacity of the buffer"
	
	^ maxCapacity
]

{ #category : 'initialization' }
CTBuffer >> initialize [

	"Initialize the buffer with default capacity"
	
	super initialize.
	self initializeWithCapacity: 10
]

{ #category : 'initialization' }
CTBuffer >> initializeWithCapacity: anInteger [

	"Initialize the buffer with specified capacity"
	
	maxCapacity := anInteger.
	elements := Array new: maxCapacity.
	readIndex := 1.
	writeIndex := 1.
	currentSize := 0
]

{ #category : 'testing' }
CTBuffer >> isEmpty [

		"Return true if buffer has no elements"
	
	^ currentSize = 0
]

{ #category : 'testing' }
CTBuffer >> isFull [

		"Return true if buffer is at maximum capacity"
	
	^ currentSize = maxCapacity
]

{ #category : 'instance creation' }
CTBuffer >> new [

		"Create a new buffer with default capacity of 10"
	
	^ self withCapacity: 10
]

{ #category : 'accessing' }
CTBuffer >> size [

		"Return the current number of elements in the buffer"
	
	^ currentSize
]

{ #category : 'instance creation' }
CTBuffer >> withCapacity: anInteger [

	"Create a new buffer with specified capacity"
	
	anInteger < 1 ifTrue: [ self error: 'Capacity must be positive' ].
	^ self new
		initializeWithCapacity: anInteger;
		yourself
]
