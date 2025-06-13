"
I am a LIFO circular buffer with fixed capacity.
I provide last-in, first-out ordering for elements.
"
Class {
	#name : 'CTLIFOBuffer',
	#superclass : 'CTAbstractBuffer',
	#category : 'Containers-Buffer',
	#package : 'Containers-Buffer'
}

{ #category : 'enumerating' }
CTLIFOBuffer >> do: aBlock [

	"Iterate over all elements from newest to oldest"
	
	| index |
	self isEmpty ifTrue: [ ^ self ].
	
	index := readIndex.
	currentSize timesRepeat: [
		aBlock value: (elements at: index).
		index := index = 1 
			ifTrue: [ capacity ]
			ifFalse: [ index - 1 ]
	]
]

{ #category : 'private' }
CTLIFOBuffer >> updateReadIndex [

	"In LIFO, readIndex moves to next newest element after pop"
	
	readIndex := readIndex = 1 
		ifTrue: [ capacity ]
		ifFalse: [ readIndex - 1 ]
]

{ #category : 'private' }
CTLIFOBuffer >> updateReadIndexForAdd [

	"In LIFO, readIndex moves to the newly added element"
	
	readIndex := writeIndex
]

{ #category : 'private' }
CTLIFOBuffer >> updateReadIndexForOverwrite [

	"In LIFO, readIndex points to the newly added element (newest)"
	
	readIndex := writeIndex
]

{ #category : 'private' }
CTLIFOBuffer >> updateWriteIndexAfterPop: poppedIndex [

	"Update writeIndex after popping an element - subclass responsibility"
	writeIndex := poppedIndex
]
