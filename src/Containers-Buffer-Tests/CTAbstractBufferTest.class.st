"
Comprehensive test suite for CTBuffer classes including CTFIFOBuffer and CTLIFOBuffer.
"
Class {
	#name : 'CTAbstractBufferTest',
	#superclass : 'TestCase',
	#instVars : [
		'buffer'
	],
	#category : 'Containers-Buffer-Tests',
	#package : 'Containers-Buffer-Tests'
}

{ #category : 'testing' }
CTAbstractBufferTest class >> isAbstract [

	^ self == CTAbstractBufferTest
]

{ #category : 'tests' }
CTAbstractBufferTest >> testAvailableSpace [

	self assert: buffer availableSpace equals: 3.
	buffer push: 'a'.
	self assert: buffer availableSpace equals: 2.
	buffer push: 'b'; push: 'c'.
	self assert: buffer availableSpace equals: 0.
	buffer pop.
	self assert: buffer availableSpace equals: 1
]

{ #category : 'tests' }
CTAbstractBufferTest >> testBufferCreationWithCapacity [

	| testBuffer |
	testBuffer := buffer class withCapacity: 5.
	
	self assert: testBuffer capacity equals: 5.
	self assert: testBuffer size equals: 0.
	self assert: testBuffer isEmpty.
	self deny: testBuffer isFull.
	self assert: testBuffer availableSpace equals: 5
]

{ #category : 'tests' }
CTAbstractBufferTest >> testBufferCreationWithInvalidCapacity [

	self should: [ buffer class withCapacity: 0 ] raise: Error.
	self should: [ buffer class withCapacity: -1 ] raise: Error
]

{ #category : 'tests' }
CTAbstractBufferTest >> testClearBuffer [

	buffer push: 'a'; push: 'b'; push: 'c'.
	buffer clear.
	
	self assert: buffer isEmpty.
	self assert: buffer size equals: 0.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 1.
	self assert: buffer availableSpace equals: 3
]

{ #category : 'tests' }
CTAbstractBufferTest >> testDefaultBufferCreation [

	| testBuffer |
	testBuffer := buffer class new.
	
	self assert: testBuffer capacity equals: 10.
	self assert: testBuffer isEmpty.
	self assert: testBuffer readIndex equals: 1.
	self assert: testBuffer writeIndex equals: 1
]

{ #category : 'tests' }
CTAbstractBufferTest >> testEmptyBufferErrors [

	self should: [ buffer peek ] raise: Error.
	self should: [ buffer pop ] raise: Error
]

{ #category : 'tests' }
CTAbstractBufferTest >> testIndicesAfterClear [

	buffer push: 'a'; push: 'b'; push: 'c'.
	buffer pop.
	buffer clear.
	
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 1
]

{ #category : 'tests' }
CTAbstractBufferTest >> testIsEmpty [

	self assert: buffer isEmpty.
	buffer push: 'test'.
	self deny: buffer isEmpty.
	buffer pop.
	self assert: buffer isEmpty
]

{ #category : 'tests' }
CTAbstractBufferTest >> testIsFull [

	self deny: buffer isFull.
	buffer push: 'a'; push: 'b'; push: 'c'.
	self assert: buffer isFull.
	buffer pop.
	self deny: buffer isFull
]

{ #category : 'tests' }
CTAbstractBufferTest >> testOverwriteBehaviorWithAvailableSpace [

	buffer push: 'a'; push: 'b'; push: 'c'.
	self assert: buffer availableSpace equals: 0.
	
	buffer push: 'd'.
	self assert: buffer availableSpace equals: 0.
	self assert: buffer size equals: 3
]

{ #category : 'tests' }
CTAbstractBufferTest >> testPopAndPushSequence [

	buffer
		push: 'a';
		push: 'b'.
	buffer pop.
	buffer push: 'c'.

	self assert: buffer size equals: 2.
	self deny: buffer isEmpty 
]

{ #category : 'tests' }
CTAbstractBufferTest >> testPushAllCollection [

		| result |
	result := buffer pushAll: #('a' 'b' 'c').
	
	self assert: buffer size equals: 3.
	self assert: result equals: 'c'.
	self assert: buffer isFull
]

{ #category : 'tests' }
CTAbstractBufferTest >> testPushMultipleElements [

	buffer push: 'first'; push: 'second'; push: 'third'.
	
	self assert: buffer size equals: 3.
	self assert: buffer isFull.
	self assert: buffer availableSpace equals: 0.
	self assert: buffer writeIndex equals: 1.
	self assert: buffer readIndex equals: 3
]

{ #category : 'tests' }
CTAbstractBufferTest >> testPushSingleElement [

	buffer push: 'first'.
	
	self assert: buffer size equals: 1.
	self deny: buffer isEmpty.
	self deny: buffer isFull.
	self assert: buffer availableSpace equals: 2.
	self assert: buffer writeIndex equals: 2.
	self assert: buffer readIndex equals: 1
]

{ #category : 'tests' }
CTAbstractBufferTest >> testSize [

	self assert: buffer size equals: 0.
	buffer push: 'a'.
	self assert: buffer size equals: 1.
	buffer push: 'b'; push: 'c'.
	self assert: buffer size equals: 3.
	buffer pop.
	self assert: buffer size equals: 2
]
