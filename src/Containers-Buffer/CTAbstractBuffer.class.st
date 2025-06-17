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

	capacity := anInteger.
	elements := Array new: capacity
]

{ #category : 'actions' }
CTAbstractBuffer >> clear [

		"Remove all elements from the buffer"
	
	1 to: capacity do: [ :i | elements at: i put: nil ].
	readIndex := 1.
	writeIndex := 1.
	currentSize := 0
]

{ #category : 'copying' }
CTAbstractBuffer >> copy [

	"Return a copy of the buffer with same contents"
	
	| copy |
	copy := self class withCapacity: capacity.
	self do: [ :each | copy push: each ].
	^ copy
]

{ #category : 'enumerating' }
CTAbstractBuffer >> do: aBlock [

	^ self subclassResponsibility
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

{ #category : 'testing' }
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

{ #category : 'testing' }
CTAbstractBuffer >> pushAll: aCollection [

	aCollection do: [ :e | self push: e ].
	^ aCollection isEmpty ifFalse: [ aCollection last ]
]

{ #category : 'accessing' }
CTAbstractBuffer >> readIndex [
	

	^ readIndex
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
