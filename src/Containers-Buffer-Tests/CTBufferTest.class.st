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
CTBufferTest >> testDefaultBufferCreation [

	| buffer |
	buffer := CTFIFOBuffer new.
	
	self assert: buffer capacity equals: 10.
	self assert: buffer isEmpty.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 1
]
