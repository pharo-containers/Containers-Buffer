"
A CTBufferTest is a test class for testing the behavior of CTBuffer
"
Class {
	#name : 'CTBufferTest',
	#superclass : 'TestCase',
	#instVars : [
		'fifoBuffer',
		'lifoBuffer'
	],
	#category : 'Containers-Buffer-Tests',
	#package : 'Containers-Buffer-Tests'
}

{ #category : 'running' }
CTBufferTest >> setUp [
super setUp.
	fifoBuffer := CTFIFOBuffer withCapacity: 3.
	lifoBuffer := CTLIFOBuffer withCapacity: 3
]

{ #category : 'tests' }
CTBufferTest >> testAvailableSpace [

	self assert: fifoBuffer availableSpace equals: 3.
	self assert: lifoBuffer availableSpace equals: 3.

	fifoBuffer push: 'a'.
	lifoBuffer push: 'x'.
	self assert: fifoBuffer availableSpace equals: 2.
	self assert: lifoBuffer availableSpace equals: 2.

	fifoBuffer
		push: 'b';
		push: 'c'.
	lifoBuffer
		push: 'y';
		push: 'z'.
	self assert: fifoBuffer availableSpace equals: 0.
	self assert: lifoBuffer availableSpace equals: 0.

	fifoBuffer pop.
	lifoBuffer pop.
	self assert: fifoBuffer availableSpace equals: 1.
	self assert: lifoBuffer availableSpace equals: 1
]

{ #category : 'tests' }
CTBufferTest >> testBrowserHistory [

	| browserHistory visitedPages |
	browserHistory := CTLIFOBuffer withCapacity: 3.
	visitedPages := OrderedCollection new.

	browserHistory push: 'https://google.com'.
	browserHistory push: 'https://github.com/pharo-containers'.
	browserHistory push: 'https://stackoverflow.com'.
	browserHistory push: 'https://reddit.com'.

	visitedPages add: browserHistory pop.
	self assert: visitedPages last equals: 'https://reddit.com'.

	browserHistory push: 'https://pharo.org'.

	[ browserHistory isEmpty ] whileFalse: [
		visitedPages add: browserHistory pop ].

	self assert: visitedPages size equals: 4.
	self assert: visitedPages first equals: 'https://reddit.com'.
	self assert: (visitedPages at: 2) equals: 'https://pharo.org'.
	self assert: (visitedPages at: 3) equals: 'https://stackoverflow.com'.
	self
		assert: visitedPages last
		equals: 'https://github.com/pharo-containers'
]

{ #category : 'tests' }
CTBufferTest >> testBufferCreationWithCapacity [

	
	| buffer |
	buffer := CTFIFOBuffer withCapacity: 5.
	
	self assert: buffer capacity equals: 5.
	self assert: buffer size equals: 0.
	self assert: buffer isEmpty.
	self deny: buffer isFull.
	self assert: buffer availableSpace equals: 5
]

{ #category : 'tests' }
CTBufferTest >> testBufferCreationWithInvalidCapacity [
	
	self should: [ CTFIFOBuffer withCapacity: 0 ] raise: Error.
	self should: [ CTFIFOBuffer withCapacity: -1 ] raise: Error.
	self should: [ CTLIFOBuffer withCapacity: 0 ] raise: Error
]

{ #category : 'tests' }
CTBufferTest >> testChatMessageQueue [

	| chatQueue displayedMessages |
	chatQueue := CTFIFOBuffer withCapacity: 4.
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
CTBufferTest >> testClearAfterOverwrite [	

	fifoBuffer
		push: 'a';
		push: 'b';
		push: 'c';
		push: 'd'.
	lifoBuffer
		push: 'x';
		push: 'y';
		push: 'z';
		push: 'w'.

	fifoBuffer clear.
	lifoBuffer clear.

	self assert: fifoBuffer isEmpty.
	self assert: lifoBuffer isEmpty.
	self assert: fifoBuffer size equals: 0.
	self assert: lifoBuffer size equals: 0.
	self assert: fifoBuffer availableSpace equals: 3.
	self assert: lifoBuffer availableSpace equals: 3.

	fifoBuffer push: 'new'.
	lifoBuffer push: 'new'.
	self assert: fifoBuffer peek equals: 'new'.
	self assert: lifoBuffer peek equals: 'new'
]

{ #category : 'tests' }
CTBufferTest >> testClearBuffer [

	fifoBuffer push: 'a'; push: 'b'; push: 'c'.
	fifoBuffer clear.
	
	self assert: fifoBuffer isEmpty.
	self assert: fifoBuffer size equals: 0.
	self assert: fifoBuffer readIndex equals: 1.
	self assert: fifoBuffer writeIndex equals: 1.
	self assert: fifoBuffer availableSpace equals: 3
]

{ #category : 'tests' }
CTBufferTest >> testCopyBuffer [

	| copy |
	fifoBuffer push: 'a'; push: 'b'; push: 'c'.
	copy := fifoBuffer copy.
	
	self assert: copy class equals: CTFIFOBuffer.
	self assert: copy capacity equals: fifoBuffer capacity.
	self assert: copy size equals: fifoBuffer size.
	self deny: copy identicalTo: fifoBuffer.
	
	"Test that copy has same contents and order"
	self assert: copy pop equals: 'a'.
	self assert: copy pop equals: 'b'.
	self assert: copy pop equals: 'c'.
	
	"Original should be unchanged"
	self assert: fifoBuffer size equals: 3
]

{ #category : 'tests' }
CTBufferTest >> testDefaultBufferCreation [

	| buffer |
	buffer := CTFIFOBuffer new.
	
	self assert: buffer capacity equals: 10.
	self assert: buffer isEmpty.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 1
]

{ #category : 'tests' }
CTBufferTest >> testDoIteration [

	| fifoElements lifoElements |
	"FIFO: Iterates oldest to newest"
	fifoBuffer
		push: 'first';
		push: 'second';
		push: 'third'.
	fifoElements := OrderedCollection new.
	fifoBuffer do: [ :each | fifoElements add: each ].
	self
		assert: fifoElements asArray
		equals: #( 'first' 'second' 'third' ).

	"LIFO: Iterates newest to oldest"
	lifoBuffer
		push: 'first';
		push: 'second';
		push: 'third'.
	lifoElements := OrderedCollection new.
	lifoBuffer do: [ :each | lifoElements add: each ].
	self
		assert: lifoElements asArray
		equals: #( 'third' 'second' 'first' )
]

{ #category : 'tests' }
CTBufferTest >> testDoIterationAfterOverwrite [

	| fifoElements lifoElements |
	
	"FIFO: After Overwrite"
	fifoBuffer push: 'a'; push: 'b'; push: 'c'.
	fifoBuffer push: 'd'.  "Overwrites 'a'"
	fifoElements := OrderedCollection new.
	fifoBuffer do: [ :each | fifoElements add: each ].
	self assert: fifoElements asArray equals: #('b' 'c' 'd').

	"LIFO: After Overwrite"
	lifoBuffer push: 'a'; push: 'b'; push: 'c'.
	lifoBuffer push: 'd'.  "Overwrites 'a'"
	lifoElements := OrderedCollection new.
	lifoBuffer do: [ :each | lifoElements add: each ].
	self assert: lifoElements asArray equals: #('d' 'c' 'b')
]

{ #category : 'tests' }
CTBufferTest >> testEmptyBufferErrors [

	
	self should: [ fifoBuffer peek ] raise: Error.
	self should: [ fifoBuffer pop ] raise: Error.
	self should: [ lifoBuffer peek ] raise: Error.
	self should: [ lifoBuffer pop ] raise: Error
]

{ #category : 'tests' }
CTBufferTest >> testIndicesAfterClear [

	fifoBuffer
		push: 'a';
		push: 'b';
		push: 'c'.
	lifoBuffer
		push: 'x';
		push: 'y';
		push: 'z'.

	fifoBuffer pop.
	lifoBuffer pop.

	fifoBuffer clear.
	lifoBuffer clear.

	self assert: fifoBuffer readIndex equals: 1.
	self assert: fifoBuffer writeIndex equals: 1.
	self assert: lifoBuffer readIndex equals: 1.
	self assert: lifoBuffer writeIndex equals: 1
]

{ #category : 'tests' }
CTBufferTest >> testIsEmpty [

	self assert: fifoBuffer isEmpty.
	self assert: lifoBuffer isEmpty.

	fifoBuffer push: 'test'.
	lifoBuffer push: 'test'.
	self deny: fifoBuffer isEmpty.
	self deny: lifoBuffer isEmpty.

	fifoBuffer pop.
	lifoBuffer pop.
	self assert: fifoBuffer isEmpty.
	self assert: lifoBuffer isEmpty
]

{ #category : 'tests' }
CTBufferTest >> testIsFull [

	self deny: fifoBuffer isFull.
	self deny: lifoBuffer isFull.

	fifoBuffer
		push: 'a';
		push: 'b';
		push: 'c'.
	lifoBuffer
		push: 'x';
		push: 'y';
		push: 'z'.

	self assert: fifoBuffer isFull.
	self assert: lifoBuffer isFull.

	fifoBuffer pop.
	lifoBuffer pop.
	self deny: fifoBuffer isFull.
	self deny: lifoBuffer isFull
]

{ #category : 'tests' }
CTBufferTest >> testOverwriteBehavior [

	"FIFO: Overwrites oldest, readIndex points to next oldest"
	fifoBuffer push: 'first'; push: 'second'; push: 'third'.
	fifoBuffer push: 'fourth'.
	self assert: fifoBuffer size equals: 3.
	self assert: fifoBuffer isFull.
	self assert: fifoBuffer peek equals: 'second'.  "First was overwritten"
	self assert: fifoBuffer readIndex equals: 2.
	self assert: fifoBuffer writeIndex equals: 2.
	
	"LIFO: Overwrites oldest, readIndex points to newest"
	lifoBuffer push: 'first'; push: 'second'; push: 'third'.
	lifoBuffer push: 'fourth'.
	self assert: lifoBuffer size equals: 3.
	self assert: lifoBuffer isFull.
	self assert: lifoBuffer peek equals: 'fourth'.  "Newest element"
	self assert: lifoBuffer readIndex equals: 1.
	self assert: lifoBuffer writeIndex equals: 2
]

{ #category : 'tests' }
CTBufferTest >> testOverwriteBehaviorWithAvailableSpace [

	fifoBuffer
		push: 'a';
		push: 'b';
		push: 'c'.
	lifoBuffer
		push: 'x';
		push: 'y';
		push: 'z'.

	self assert: fifoBuffer availableSpace equals: 0.
	self assert: lifoBuffer availableSpace equals: 0.

	fifoBuffer push: 'd'.
	lifoBuffer push: 'w'.

	self assert: fifoBuffer availableSpace equals: 0.
	self assert: lifoBuffer availableSpace equals: 0.
	self assert: fifoBuffer size equals: 3.
	self assert: lifoBuffer size equals: 3
]

{ #category : 'tests' }
CTBufferTest >> testPeekAndPop [

	"FIFO: Oldest First"
	fifoBuffer push: 'first'; push: 'second'; push: 'third'.
	self assert: fifoBuffer peek equals: 'first'.
	self assert: fifoBuffer size equals: 3.
	self assert: fifoBuffer pop equals: 'first'.
	self assert: fifoBuffer size equals: 2.
	self assert: fifoBuffer peek equals: 'second'.
	
	"LIFO: Newest First"
	lifoBuffer push: 'first'; push: 'second'; push: 'third'.
	self assert: lifoBuffer peek equals: 'third'.
	self assert: lifoBuffer size equals: 3.
	self assert: lifoBuffer pop equals: 'third'.
	self assert: lifoBuffer size equals: 2.
	self assert: lifoBuffer peek equals: 'second'
]

{ #category : 'tests' }
CTBufferTest >> testPopAndPushSequence [

	fifoBuffer
		push: 'a';
		push: 'b'.
	fifoBuffer pop.
	fifoBuffer push: 'c'.

	self assert: fifoBuffer size equals: 2.
	self assert: fifoBuffer pop equals: 'b'.
	self assert: fifoBuffer pop equals: 'c'.
	self assert: fifoBuffer isEmpty.

	lifoBuffer
		push: 'x';
		push: 'y'.
	lifoBuffer pop.
	lifoBuffer push: 'z'.

	self assert: lifoBuffer size equals: 2.
	self assert: lifoBuffer pop equals: 'z'.
	self assert: lifoBuffer pop equals: 'x'.
	self assert: lifoBuffer isEmpty
]

{ #category : 'tests' }
CTBufferTest >> testPushAllCollection [

	| result |
	result := fifoBuffer pushAll: #('a' 'b' 'c').
	
	self assert: fifoBuffer size equals: 3.
	self assert: result equals: 'c'.  "Returns last element"
	self assert: fifoBuffer isFull
]

{ #category : 'tests' }
CTBufferTest >> testPushMultipleElements [

	fifoBuffer push: 'first'; push: 'second'; push: 'third'.
	
	self assert: fifoBuffer size equals: 3.
	self assert: fifoBuffer isFull.
	self assert: fifoBuffer availableSpace equals: 0.
	self assert: fifoBuffer writeIndex equals: 1.
	self assert: fifoBuffer readIndex equals: 1
]

{ #category : 'tests' }
CTBufferTest >> testPushSingleElement [

	fifoBuffer push: 'first'.
	
	self assert: fifoBuffer size equals: 1.
	self deny: fifoBuffer isEmpty.
	self deny: fifoBuffer isFull.
	self assert: fifoBuffer availableSpace equals: 2.
	self assert: fifoBuffer writeIndex equals: 2.
	self assert: fifoBuffer readIndex equals: 1
]

{ #category : 'tests' }
CTBufferTest >> testReadIndexAfterPop [

		"FIFO: readIndex moves to next oldest"
	fifoBuffer push: 'a'; push: 'b'; push: 'c'.
	fifoBuffer pop.
	self assert: fifoBuffer readIndex equals: 2.
	
	"LIFO: readIndex moves to next newest"  
	lifoBuffer push: 'x'; push: 'y'; push: 'z'.
	lifoBuffer pop.
	self assert: lifoBuffer readIndex equals: 2
]

{ #category : 'tests' }
CTBufferTest >> testReadIndexBehaviorOnAdd [

	"FIFO: readIndex stays on oldest"
	fifoBuffer push: 'first'.
	self assert: fifoBuffer readIndex equals: 1.
	fifoBuffer push: 'second'. 
	self assert: fifoBuffer readIndex equals: 1.
	fifoBuffer push: 'third'.
	self assert: fifoBuffer readIndex equals: 1.
	
	"LIFO: readIndex moves to newest"
	lifoBuffer push: 'first'.
	self assert: lifoBuffer readIndex equals: 1.
	lifoBuffer push: 'second'.
	self assert: lifoBuffer readIndex equals: 2.
	lifoBuffer push: 'third'.
	self assert: lifoBuffer readIndex equals: 3
]

{ #category : 'tests' }
CTBufferTest >> testReadIndexBehaviorOnOverwrite [
	"FIFO: readIndex moves away from overwritten element"

	fifoBuffer
		push: 'a';
		push: 'b';
		push: 'c'.
	self assert: fifoBuffer readIndex equals: 1.
	fifoBuffer push: 'd'.
	self assert: fifoBuffer readIndex equals: 2.
	fifoBuffer push: 'e'.
	self assert: fifoBuffer readIndex equals: 3.

	"LIFO: readIndex points to newest element"
	lifoBuffer
		push: 'a';
		push: 'b';
		push: 'c'.
	self assert: lifoBuffer readIndex equals: 3.
	lifoBuffer push: 'd'.
	self assert: lifoBuffer readIndex equals: 1.
	lifoBuffer push: 'e'.
	self assert: lifoBuffer readIndex equals: 2
]

{ #category : 'tests' }
CTBufferTest >> testSize [

	self assert: fifoBuffer size equals: 0.
	self assert: lifoBuffer size equals: 0.

	fifoBuffer push: 'a'.
	lifoBuffer push: 'x'.
	self assert: fifoBuffer size equals: 1.
	self assert: lifoBuffer size equals: 1.

	fifoBuffer
		push: 'b';
		push: 'c'.
	lifoBuffer
		push: 'y';
		push: 'z'.
	self assert: fifoBuffer size equals: 3.
	self assert: lifoBuffer size equals: 3.

	fifoBuffer pop.
	lifoBuffer pop.
	self assert: fifoBuffer size equals: 2.
	self assert: lifoBuffer size equals: 2
]

{ #category : 'tests' }
CTBufferTest >> testWriteIndexAfterPopAndOverwrite [

	| writeIndexAfterPop |
	fifoBuffer
		push: 'a';
		push: 'b';
		push: 'c'.
	fifoBuffer pop.

	writeIndexAfterPop := fifoBuffer writeIndex.

	fifoBuffer push: 'd'.
	fifoBuffer push: 'e'.

	self assert: fifoBuffer size equals: 3.

	lifoBuffer
		push: 'x';
		push: 'y';
		push: 'z'.
	lifoBuffer pop.	
	lifoBuffer push: 'w'.

	self assert: lifoBuffer size equals: 3.
	self assert: lifoBuffer peek equals: 'w'
]

{ #category : 'tests' }
CTBufferTest >> testWriteIndexBehavior [

	self assert: fifoBuffer writeIndex equals: 1.
	self assert: lifoBuffer writeIndex equals: 1.

	fifoBuffer push: 'a'.
	lifoBuffer push: 'x'.
	self assert: fifoBuffer writeIndex equals: 2.
	self assert: lifoBuffer writeIndex equals: 2.

	fifoBuffer
		push: 'b';
		push: 'c'.
	lifoBuffer
		push: 'y';
		push: 'z'.
	self assert: fifoBuffer writeIndex equals: 1.
	self assert: lifoBuffer writeIndex equals: 1
]
