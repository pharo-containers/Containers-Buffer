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
CTAbstractBuffer class >> new [

	"Create a new buffer with default capacity"

	^ self new: 10
]

{ #category : 'instance creation' }
CTAbstractBuffer class >> new: anInteger [
	"Create a new buffer with the specified capacity"

	anInteger < 1 ifTrue: [ self error: 'Capacity must be positive' ].
	^ self basicNew
		  setupWithCapacity: anInteger;
		  yourself
]

{ #category : 'adding' }
CTAbstractBuffer >> add: anObject [

	^ self push: anObject 
]

{ #category : 'adding' }
CTAbstractBuffer >> addAll: aCollection [

	^ self pushAll: aCollection 
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

{ #category : 'copying' }
CTAbstractBuffer >> copy [

	^ self subclassResponsibility
]

{ #category : 'enumerating' }
CTAbstractBuffer >> do: aBlock [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
CTAbstractBuffer >> elements [

	^ elements
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
CTAbstractBuffer >> peek [

		"Return next element to be retrieved without removing it"
	
	self isEmpty ifTrue: [ self error: 'Buffer is empty' ].
	^ elements at: readIndex
]

{ #category : 'removing' }
CTAbstractBuffer >> pop [
	"Remove and return element from readIndex position"

	| element poppedIndex|
	self isEmpty ifTrue: [ self error: 'Buffer is empty' ].
	
	element := elements at: readIndex.
	elements at: readIndex put: nil.
	poppedIndex := readIndex.

	self updateReadIndex.
	self updateWriteIndexAfterPop: poppedIndex.
	currentSize := currentSize - 1.
	^ element
]

{ #category : 'adding' }
CTAbstractBuffer >> push: anObject [
	"Add an element to the buffer"

	elements at: writeIndex put: anObject.
	
	currentSize = 0
		ifTrue: [
			"Very first element - both FIFO and LIFO point readIndex to it"
			readIndex := writeIndex.
			currentSize := 1.
		]
		ifFalse: [
			self isFull
				ifTrue: [
					"Buffer is full - we just overwrote an element"
					self updateReadIndexForOverwrite.
				]
				ifFalse: [
					"Normal case - adding to non-full buffer"
					self updateReadIndexForAdd.
					currentSize := currentSize + 1.
				].
		].
	
	writeIndex := writeIndex \\ capacity + 1.
	^ anObject
]

{ #category : 'adding' }
CTAbstractBuffer >> pushAll: aCollection [

	aCollection do: [ :e | self push: e ].
	^ aCollection isEmpty ifFalse: [ aCollection last ]
]

{ #category : 'accessing' }
CTAbstractBuffer >> readIndex [
	

	^ readIndex
]

{ #category : 'removing' }
CTAbstractBuffer >> removeAll [

	"Remove all elements from the buffer"
	
	1 to: capacity do: [ :i | elements at: i put: nil ].
	readIndex := 1.
	writeIndex := 1.
	currentSize := 0
]

{ #category : 'private' }
CTAbstractBuffer >> setupWithCapacity: anInteger [

	capacity := anInteger.
	elements := Array new: capacity.
	readIndex := 1.
	writeIndex := 1.
	currentSize := 0
]

{ #category : 'accessing' }
CTAbstractBuffer >> size [

	^ currentSize
]

{ #category : 'private' }
CTAbstractBuffer >> updateReadIndex [

		"Update readIndex after pop or overwrite - subclass responsibility"
	
	self subclassResponsibility
]

{ #category : 'private' }
CTAbstractBuffer >> updateReadIndexForAdd [

	"Update readIndex when adding new element - subclass responsibility"
	
	self subclassResponsibility
]

{ #category : 'private' }
CTAbstractBuffer >> updateReadIndexForOverwrite [

	"Update readIndex when overwriting an element - subclass responsibility"
	
	self subclassResponsibility
]

{ #category : 'private' }
CTAbstractBuffer >> updateWriteIndexAfterPop: poppedIndex [

	"Update writeIndex after popping an element - subclass responsibility"
	
	self subclassResponsibility
]

{ #category : 'accessing' }
CTAbstractBuffer >> writeIndex [ 

	^ writeIndex
]
