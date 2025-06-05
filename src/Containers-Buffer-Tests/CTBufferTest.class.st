"
A CTBufferTest is a test class for testing the behavior of CTBuffer
"
Class {
	#name : 'CTBufferTest',
	#superclass : 'TestCase',
	#instVars : [
		'buffer'
	],
	#category : 'Containers-Buffer-Tests',
	#package : 'Containers-Buffer-Tests'
}

{ #category : 'running' }
CTBufferTest >> setUp [
	super setUp.
	buffer := CTBuffer new

]

{ #category : 'tests' }
CTBufferTest >> testEmptyBuffer [

	"Test operations on empty buffer"
	
	self assert: buffer isEmpty.
	self should: [ buffer get ] raise: Error
]

{ #category : 'tests' }
CTBufferTest >> testGet [
	"Test getting elements from buffer"

	| result |
	
	buffer put: 'test'.
	result := buffer get.
	self assert: result equals: 'test'.
	self assert: buffer isEmpty
]

{ #category : 'tests' }
CTBufferTest >> testPut [

	"Test adding elements to buffer"
	
	buffer put: 'first'.
	self deny: buffer isEmpty.
	
	buffer put: 'second'.
	self deny: buffer isEmpty
]
