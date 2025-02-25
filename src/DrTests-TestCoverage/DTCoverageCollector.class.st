"
I am a specialized `CoverageCollector` for DrTests.

I specify some changes to better fit the needs of the DrTests coverage plugin and display the source code in a panel by highlighting the covered lines of code.

The main change is in `DTCoverageCollector>>#collectResult` where I omit the ""reset"" step.
"
Class {
	#name : 'DTCoverageCollector',
	#superclass : 'CoverageCollector',
	#category : 'DrTests-TestCoverage',
	#package : 'DrTests-TestCoverage'
}

{ #category : 'basic' }
DTCoverageCollector >> basicCollectResult [
	"Collect the results but does not reset the annotations."

	| res |
	res := DTCoverageResult new.
	res collector: self.
	res methods: (methods select: [ :m | m ast hasBeenExecuted ]).
	res nodes: IdentitySet new.
	nodes do: [ :node |
		node hasBeenExecuted ifTrue: [ res nodes add: node ] ].
	res partiallyCoveredMethods: (methods select: [ :meth |
			 meth ast hasBeenExecuted and: [ (self hasAllNodesCovered: meth) not ] ]).
	^ res
]

{ #category : 'basic' }
DTCoverageCollector >> collectResult [
	"Collect the results.
	This also remaps the methods and  NOT resets the annotations."

	self remapMethods.
	^ self basicCollectResult
]

{ #category : 'precalculations' }
DTCoverageCollector >> hasAllNodesCovered: aMethod [

	aMethod ast nodesDo: [ :node |
		node isSequence ifTrue: [ node hasBeenExecuted ifFalse: [ ^ false ] ] ].
	^ true
]
