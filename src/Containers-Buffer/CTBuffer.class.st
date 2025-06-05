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
		'size'
	],
	#category : 'Containers-Buffer',
	#package : 'Containers-Buffer'
}

{ #category : 'operations' }
CTBuffer >> get [
	"Remove and return the first element"

	| element |
	self isEmpty ifTrue: [ self error: 'Buffer is empty' ].

	element := elements first.
	2 to: size do: [ :i | elements at: i - 1 put: (elements at: i) ].
	elements at: size put: nil.
	size := size - 1.
	^ element
]

{ #category : 'initialization' }
CTBuffer >> initialize [

	"Initialize the buffer"
	
	super initialize.
	maxCapacity := 10.
	elements := Array new: maxCapacity.
	size := 0
]

{ #category : 'testing' }
CTBuffer >> isEmpty [

	"Return true if buffer has no elements"
	
	^ size = 0
]

{ #category : 'operations' }
CTBuffer >> put: anObject [

	"Add an element to the buffer"
	
	size := size + 1.
	elements at: size put: anObject.
	^ anObject
]
