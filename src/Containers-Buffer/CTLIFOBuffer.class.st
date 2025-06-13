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
