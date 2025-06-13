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
CTBufferTest >> testDefaultBufferCreation [

	| buffer |
	buffer := CTFIFOBuffer new.
	
	self assert: buffer capacity equals: 10.
	self assert: buffer isEmpty.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 1
]

{ #category : 'tests' }
CTBufferTest >> testEmptyBufferErrors [

	
	self should: [ fifoBuffer peek ] raise: Error.
	self should: [ fifoBuffer pop ] raise: Error.
	self should: [ lifoBuffer peek ] raise: Error.
	self should: [ lifoBuffer pop ] raise: Error
]

{ #category : 'tests' }
CTBufferTest >> testMultipleOverwrites [

	"FIFO: Maintains oldest-to-newest order"
	fifoBuffer push: 'a'; push: 'b'; push: 'c'.
	fifoBuffer push: 'd'; push: 'e'; push: 'f'.
	self assert: fifoBuffer size equals: 3.
	self assert: fifoBuffer pop equals: 'd'.
	self assert: fifoBuffer pop equals: 'e'.
	self assert: fifoBuffer pop equals: 'f'.
	self assert: fifoBuffer isEmpty.

	"LIFO: Maintains newest-to-oldest order"
	lifoBuffer push: 'a'; push: 'b'; push: 'c'.
	lifoBuffer push: 'd'; push: 'e'; push: 'f'.
	self assert: lifoBuffer size equals: 3.
	self assert: lifoBuffer pop equals: 'f'.
	self assert: lifoBuffer pop equals: 'e'.
	self assert: lifoBuffer pop equals: 'd'.
	self assert: lifoBuffer isEmpty
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
