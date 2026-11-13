"
I am a resolver for tests: I resolve as MetacelloInteractiveResolver does, but say that the baselines of the projects referenced were read without being installed, as a resolver reading them in a sandbox would.
"
Class {
	#name : 'MetacelloBaselineReadingResolver',
	#superclass : 'MetacelloInteractiveResolver',
	#category : 'Metacello-Tests-Resolution',
	#package : 'Metacello-Tests',
	#tag : 'Resolution'
}

{ #category : 'resolving' }
MetacelloBaselineReadingResolver >> resolve: aProjectSpec engine: anEngine [

	| answer |
	answer := super resolve: aProjectSpec engine: anEngine.
	answer projects do: [ :each | (answer roots includes: each) ifFalse: [ each beInstallingBaseline ] ].
	^ answer
]
