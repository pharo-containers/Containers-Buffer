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
CTBufferTest >> testAvailableSize [

    buffer := CTBuffer withCapacity: 10.
    buffer
        put: 4;
        put: 5.

    self assert: buffer availableSpace equals: 8
]

{ #category : 'tests' }
CTBufferTest >> testAvailableSizeWhenExcedeedCapacity [

    buffer := CTBuffer withCapacity: 4.

    1 to: 10 do: [ :i | buffer put: i ].
    self assert: buffer availableSpace equals: 0.

    buffer pop.
    self assert: buffer availableSpace equals: 1.

    buffer pop.
    self assert: buffer availableSpace equals: 2
]

{ #category : 'tests' }
CTBufferTest >> testEmptyBuffer [

    self assert: buffer isEmpty.
    self should: [ buffer get ] raise: Error
]

{ #category : 'tests' }
CTBufferTest >> testIndeces [

	buffer := CTBuffer withCapacity: 4.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 1.

	buffer put: 1.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 2.
	
	buffer put: 1.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 3.
	
	buffer put: 1.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 4.
	
	buffer put: 1.
	self assert: buffer readIndex equals: 1.
	self assert: buffer writeIndex equals: 1.

	buffer put: 1.
	self assert: buffer readIndex equals: 2.
	self assert: buffer writeIndex equals: 2.
	
	buffer pop.
	self assert: buffer readIndex equals: 3.
	self assert: buffer writeIndex equals: 2
]

{ #category : 'tests' }
CTBufferTest >> testPop [

    self assert: buffer isEmpty.
    buffer put: 'test'.
    self deny: buffer isEmpty.

    self assert: buffer pop equals: 'test'.
    self assert: buffer isEmpty
]

{ #category : 'tests' }
CTBufferTest >> testPut [

    buffer put: 'first'.
    self deny: buffer isEmpty.

    buffer pop.
    self assert: buffer isEmpty.

    buffer put: 'second'.
    self deny: buffer isEmpty
]

{ #category : 'tests' }
CTBufferTest >> testPutAll [

    buffer := CTBuffer withCapacity: 4.
    buffer putAll: (1 to: 6).

    self assert: buffer size equals: 4.

    self assertCollection: buffer elements hasSameElements: #( 5 6 3 4 ).

    self assert: buffer pop equals: 3.
    self assert: buffer pop equals: 4.
    self assert: buffer pop equals: 5.
    self assert: buffer pop equals: 6.

    self assert: buffer isEmpty
]

{ #category : 'tests' }
CTBufferTest >> testPutWhenSizeExcedeesCapacity [

    buffer := CTBuffer withCapacity: 4.
    1 to: 6 do: [ :i | buffer put: i ].

    self assert: buffer size equals: 4.

    self assertCollection: buffer elements hasSameElements: #( 5 6 3 4 ).

    self assert: buffer pop equals: 3.
    self assert: buffer pop equals: 4.
    self assert: buffer pop equals: 5.
    self assert: buffer pop equals: 6.

    self assert: buffer isEmpty
]

{ #category : 'tests' }
CTBufferTest >> testTop [

    buffer
        put: 4;
        put: 5.

    self assert: buffer top equals: 4.
    self assert: buffer size equals: 2
]
