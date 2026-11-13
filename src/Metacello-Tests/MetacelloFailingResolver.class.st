"
I am a resolver for tests: I resolve as MetacelloInteractiveResolver does, then add an error to the resolution.
"
Class {
	#name : 'MetacelloFailingResolver',
	#superclass : 'MetacelloInteractiveResolver',
	#category : 'Metacello-Tests-Resolution',
	#package : 'Metacello-Tests',
	#tag : 'Resolution'
}

{ #category : 'resolving' }
MetacelloFailingResolver >> resolve: aProjectSpec engine: anEngine [

	| answer |
	answer := super resolve: aProjectSpec engine: anEngine.
	answer addFinding: (MetacelloResolutionFinding error: #cannotResolve text: 'cannot resolve' request: answer roots first requests first).
	^ answer
]
