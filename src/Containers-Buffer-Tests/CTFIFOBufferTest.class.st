"
Test class for FIFO (First In, First Out) buffer implementation.
Tests FIFO-specific ordering behavior and includes real-world usage examples.
"
Class {
	#name : 'CTFIFOBufferTest',
	#superclass : 'CTAbstractBufferTest',
	#category : 'Containers-Buffer-Tests',
	#package : 'Containers-Buffer-Tests'
}

{ #category : 'running' }
CTFIFOBufferTest >> setUp [

	super setUp.
	buffer := CTFIFOBuffer new: 3
]

{ #category : 'tests' }
CTFIFOBufferTest >> testChatMessageQueue [

	| chatQueue displayedMessages |
	chatQueue := CTFIFOBuffer new: 4.
	displayedMessages := OrderedCollection new.

	chatQueue push: 'Alok: Hello everyone!'.
	chatQueue push: 'Sebastian: Hey Alok!'.
	chatQueue push: 'Gordana: How is everyone doing?'.
	chatQueue push: 'Sebastian: Great to see you all!'.

	displayedMessages add: chatQueue pop.
	self assert: displayedMessages last equals: 'Alok: Hello everyone!'.

	chatQueue push: 'Alok: Sorry I was late!'.
	[ chatQueue isEmpty ] whileFalse: [
		displayedMessages add: chatQueue pop ].

	self assert: displayedMessages size equals: 5.
	self assert: (displayedMessages at: 2) equals: 'Sebastian: Hey Alok!'.
	self assert: displayedMessages last equals: 'Alok: Sorry I was late!'
]

{ #category : 'tests' }
CTFIFOBufferTest >> testCopyBufferContentsOrder [

	| copy |
	buffer push: 'a'; push: 'b'; push: 'c'.
	copy := buffer copy.
	
	self assert: copy pop equals: 'a'.
	self assert: copy pop equals: 'b'.
	self assert: copy pop equals: 'c'
]

{ #category : 'tests' }
CTFIFOBufferTest >> testDoIteration [

	| elements |
	buffer push: 'first'; push: 'second'; push: 'third'.
	elements := OrderedCollection new.
	buffer do: [ :each | elements add: each ].
	
	self assert: elements asArray equals: #('first' 'second' 'third')
]

{ #category : 'tests' }
CTFIFOBufferTest >> testDoIterationAfterOverwrite [

	| elements |
	buffer push: 'a'; push: 'b'; push: 'c'.
	buffer push: 'd'.
	
	elements := OrderedCollection new.
	buffer do: [ :each | elements add: each ].
	self assert: elements asArray equals: #('b' 'c' 'd')
]

{ #category : 'tests' }
CTFIFOBufferTest >> testOverwriteBehavior [

	buffer push: 'first'; push: 'second'; push: 'third'.
	buffer push: 'fourth'.
	
	self assert: buffer size equals: 3.
	self assert: buffer isFull.
	self assert: buffer peek equals: 'second'.
	self assert: buffer readIndex equals: 2.
	self assert: buffer writeIndex equals: 2
]

{ #category : 'tests' }
CTFIFOBufferTest >> testPeekAndPop [

	buffer push: 'first'; push: 'second'; push: 'third'.
	
	self assert: buffer peek equals: 'first'.
	self assert: buffer size equals: 3.
	self assert: buffer pop equals: 'first'.
	self assert: buffer size equals: 2.
	self assert: buffer peek equals: 'second'
]

{ #category : 'tests' }
CTFIFOBufferTest >> testPopAndPushSequence [

	buffer push: 'a'; push: 'b'.
	self assert: buffer pop equals: 'a'.
	buffer push: 'c'.
	
	self assert: buffer pop equals: 'b'.
	self assert: buffer pop equals: 'c'.
	self assert: buffer isEmpty
]

{ #category : 'tests' }
CTFIFOBufferTest >> testReadIndexAfterPop [

	buffer push: 'a'; push: 'b'; push: 'c'.
	buffer pop.
	self assert: buffer readIndex equals: 2
]

{ #category : 'tests' }
CTFIFOBufferTest >> testReadIndexBehaviorOnAdd [

	buffer push: 'first'.
	self assert: buffer readIndex equals: 1.
	buffer push: 'second'. 
	self assert: buffer readIndex equals: 1.
	buffer push: 'third'.
	self assert: buffer readIndex equals: 1
]

{ #category : 'tests' }
CTFIFOBufferTest >> testReadIndexBehaviorOnOverwrite [

	buffer push: 'a'; push: 'b'; push: 'c'.
	self assert: buffer readIndex equals: 1.
	
	buffer push: 'd'.
	self assert: buffer readIndex equals: 2.
	
	buffer push: 'e'.
	self assert: buffer readIndex equals: 3
]

{ #category : 'tests' }
CTFIFOBufferTest >> testWriteIndexAfterPopAndOverwrite [

	buffer push: 'a'; push: 'b'; push: 'c'.
	self assert: buffer writeIndex equals: 1.
	
	buffer pop.
	self assert: buffer writeIndex equals: 1.
	
	buffer push: 'd'.
	self assert: buffer writeIndex equals: 2.
	buffer push: 'e'.
	self assert: buffer writeIndex equals: 3.
	self assert: buffer size equals: 3
]

{ #category : 'tests' }
CTFIFOBufferTest >> testWriteIndexBehavior [

	self assert: buffer writeIndex equals: 1.
	buffer push: 'a'.
	self assert: buffer writeIndex equals: 2.
	buffer push: 'b'; push: 'c'.
	self assert: buffer writeIndex equals: 1
]
