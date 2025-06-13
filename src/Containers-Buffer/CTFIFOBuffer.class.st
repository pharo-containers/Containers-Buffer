"
I am a FIFO circular buffer with fixed capacity. I provide first-in, first-out ordering for elements.
"
Class {
	#name : 'CTFIFOBuffer',
	#superclass : 'CTAbstractBuffer',
	#category : 'Containers-Buffer',
	#package : 'Containers-Buffer'
}

{ #category : 'enumerating' }
CTFIFOBuffer >> do: aBlock [

	"Iterate over all elements from oldest to newest"
	
	| index |
	self isEmpty ifTrue: [ ^ self ].
	
	index := readIndex.
	currentSize timesRepeat: [
		aBlock value: (elements at: index).
		index := index \\ capacity + 1
	]
]

{ #category : 'private' }
CTFIFOBuffer >> updateReadIndex [

	"In FIFO, readIndex moves to next oldest element after pop"
	
	readIndex := readIndex \\ capacity + 1
]

{ #category : 'private' }
CTFIFOBuffer >> updateReadIndexForAdd [

	"In FIFO, readIndex stays pointing to oldest element when adding"
	
	"No change needed - readIndex keeps pointing to oldest"
]

{ #category : 'private' }
CTFIFOBuffer >> updateReadIndexForOverwrite [

		"In FIFO, when overwriting, readIndex moves to next oldest element"
	
	readIndex := readIndex \\ capacity + 1

]
