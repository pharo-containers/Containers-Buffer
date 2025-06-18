"
Test class for LIFO (Last In, First Out) buffer implementation.
Tests LIFO-specific ordering behavior and includes real-world usage examples.
"
Class {
	#name : 'CTLIFOBufferTest',
	#superclass : 'CTAbstractBufferTest',
	#category : 'Containers-Buffer-Tests',
	#package : 'Containers-Buffer-Tests'
}

{ #category : 'running' }
CTLIFOBufferTest >> setUp [
super setUp.
	buffer := CTLIFOBuffer withCapacity: 3
]

{ #category : 'tests' }
CTLIFOBufferTest >> testBrowserHistory [

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
	self assert: visitedPages last equals: 'https://github.com/pharo-containers'
]

{ #category : 'tests' }
CTLIFOBufferTest >> testCopyBufferContentsOrder [

	| copy |
	buffer push: 'a'; push: 'b'; push: 'c'.
	copy := buffer copy.
	
	self assert: copy pop equals: 'c'.
	self assert: copy pop equals: 'b'.
	self assert: copy pop equals: 'a'
]

{ #category : 'tests' }
CTLIFOBufferTest >> testDoIteration [

	| elements |
	buffer push: 'first'; push: 'second'; push: 'third'.
	elements := OrderedCollection new.
	buffer do: [ :each | elements add: each ].
	
	self assert: elements asArray equals: #('third' 'second' 'first')
]

{ #category : 'tests' }
CTLIFOBufferTest >> testDoIterationAfterOverwrite [

	| elements |
	buffer push: 'a'; push: 'b'; push: 'c'.
	buffer push: 'd'.
	
	elements := OrderedCollection new.
	buffer do: [ :each | elements add: each ].
	self assert: elements asArray equals: #('d' 'c' 'b')
]

{ #category : 'tests' }
CTLIFOBufferTest >> testOverwriteBehavior [

	buffer push: 'first'; push: 'second'; push: 'third'.
	buffer push: 'fourth'.
	
	self assert: buffer size equals: 3.
	self assert: buffer isFull.
	self assert: buffer peek equals: 'fourth'.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 2
]

{ #category : 'tests' }
CTLIFOBufferTest >> testPeekAndPop [

	buffer push: 'first'; push: 'second'; push: 'third'.
	
	self assert: buffer peek equals: 'third'.
	self assert: buffer size equals: 3.
	self assert: buffer pop equals: 'third'.
	self assert: buffer size equals: 2.
	self assert: buffer peek equals: 'second'
]

{ #category : 'tests' }
CTLIFOBufferTest >> testPopAndPushSequence [

	buffer push: 'a'; push: 'b'.
	self assert: buffer pop equals: 'b'.
	buffer push: 'c'.
	
	self assert: buffer pop equals: 'c'.
	self assert: buffer pop equals: 'a'.
	self assert: buffer isEmpty
]

{ #category : 'tests' }
CTLIFOBufferTest >> testReadIndexAfterPop [

	buffer push: 'x'; push: 'y'; push: 'z'.
	buffer pop.
	self assert: buffer readIndex equals: 2
]

{ #category : 'tests' }
CTLIFOBufferTest >> testReadIndexBehaviorOnAdd [

	buffer push: 'first'.
	self assert: buffer readIndex equals: 1.
	buffer push: 'second'.
	self assert: buffer readIndex equals: 2.
	buffer push: 'third'.
	self assert: buffer readIndex equals: 3
]

{ #category : 'tests' }
CTLIFOBufferTest >> testReadIndexBehaviorOnOverwrite [

	buffer push: 'a'; push: 'b'; push: 'c'.
	self assert: buffer readIndex equals: 3.
	
	buffer push: 'd'.
	self assert: buffer readIndex equals: 1.
	
	buffer push: 'e'.
	self assert: buffer readIndex equals: 2
]

{ #category : 'tests' }
CTLIFOBufferTest >> testWriteIndexAfterPopAndOverwrite [

	buffer push: 'a'; push: 'b'; push: 'c'.
	self assert: buffer writeIndex equals: 1.
	
	buffer pop.
	self assert: buffer writeIndex equals: 3.
	
	buffer push: 'd'.
	self assert: buffer writeIndex equals: 1.
	self assert: buffer size equals: 3.
	self assert: buffer peek equals: 'd'
]

{ #category : 'tests' }
CTLIFOBufferTest >> testWriteIndexBehavior [

	self assert: buffer writeIndex equals: 1.
	buffer push: 'a'.
	self assert: buffer writeIndex equals: 2.
	buffer push: 'b'; push: 'c'.
	self assert: buffer writeIndex equals: 1
]
