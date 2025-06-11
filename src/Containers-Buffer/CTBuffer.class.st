"
I represent a circular buffer with fixed capacity.

A Buffer provides efficient temporary storage with automatic wraparound when full. It supports O(1) insertion and removal operations, making it ideal for streaming data, producer-consumer scenarios, and situations requiring fixed-memory buffering.
"
Class {
	#name : 'CTBuffer',
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
CTBuffer class >> withCapacity: anInteger [

    anInteger < 1 ifTrue: [ self error: 'Capacity must be not negative' ].

    ^ self new
          capacity: anInteger;
          yourself
]

{ #category : 'accessing' }
CTBuffer >> availableSpace [
		"Return the number of additional elements that can be stored"
	
	^ capacity - currentSize
]

{ #category : 'accessing' }
CTBuffer >> capacity [

	"Return the maximum capacity of the buffer"
	
	^ capacity
]

{ #category : 'accessing' }
CTBuffer >> capacity: anInteger [

    capacity := anInteger.
    elements := Array new: capacity
]

{ #category : 'accessing' }
CTBuffer >> elements [
	^ elements
]

{ #category : 'initialization' }
CTBuffer >> initialize [

    super initialize.
    capacity := 10.
    elements := Array new: capacity.
    readIndex := 1.
    writeIndex := 1.
    currentSize := 0
]

{ #category : 'testing' }
CTBuffer >> isEmpty [

    ^ currentSize = 0
]

{ #category : 'testing' }
CTBuffer >> isFull [

    ^ currentSize = capacity
]

{ #category : 'actions api' }
CTBuffer >> pop [

	"Remove and return the oldest element"

	| element |
	self isEmpty ifTrue: [ self error: 'Buffer is empty' ].

	element := elements at: readIndex.
	elements at: readIndex put: nil.
	readIndex := readIndex \\ capacity + 1.
	currentSize := currentSize - 1.
	^ element
]

{ #category : 'actions api' }
CTBuffer >> put: anObject [

	"Add an element to the buffer. If full, overwrites oldest element"
	
	elements at: writeIndex put: anObject.
	writeIndex := writeIndex \\ capacity + 1.
	
	self isFull 
		ifTrue: [ readIndex := readIndex \\ capacity + 1 ]
		ifFalse: [ currentSize := currentSize + 1 ].
		
	^ anObject
]

{ #category : 'actions api' }
CTBuffer >> putAll: aCollection [

    aCollection do: [ :e | self put: e ].
    ^ aCollection last
]

{ #category : 'accessing' }
CTBuffer >> readIndex [
	^ readIndex
]

{ #category : 'accessing' }
CTBuffer >> size [

    ^ currentSize
]

{ #category : 'actions api' }
CTBuffer >> top [

    ^ elements at: readIndex
]

{ #category : 'accessing' }
CTBuffer >> writeIndex [
	^ writeIndex
]
