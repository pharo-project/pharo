"
This inherits from `DTCoverageCollector`.

I specify some changes to better fit the needs of the DrTest coverage plugin and display in a panel the source code by highlighting the covered code lines.

The main change is in the `DTCoverageCollector>>#collectResult` where I delete the ""self reset"" line.
"
Class {
	#name : #DTCoverageCollector,
	#superclass : #CoverageCollector,
	#category : #'Coverage-Base'
}

{ #category : #precalculations }
DTCoverageCollector >> hasAllNodesCovered: aMethod [

	aMethod ast nodesDo: [ :node |
		node isSequence ifTrue: [ node hasBeenExecuted ifFalse: [ ^ false ] ] ].
	^ true
]

{ #category : #basic }
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
			 meth ast hasBeenExecuted and: [ (self allNodesCovered: meth) not ] ]).
	^ res
]

{ #category : #basic }
DTCoverageCollector >> collectResult [
	"Collect the results.
	This also remaps the methods and  NOT resets the annotations."

	| res |
	self remapMethods.
	res := self basicCollectResult.
	^ res
]
