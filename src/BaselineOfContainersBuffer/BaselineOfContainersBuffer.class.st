Class {
	#name : 'BaselineOfContainersBuffer',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfContainersBuffer',
	#package : 'BaselineOfContainersBuffer'
}

{ #category : 'baselines' }
BaselineOfContainersBuffer >> baseline: spec [

	<baseline>

	spec for: #pharo do: [		
		spec package: 'Containers-Buffer'.
		spec package: 'Containers-Buffer-Tests' with: [ spec requires: #('Containers-Buffer') ]
	]
]
