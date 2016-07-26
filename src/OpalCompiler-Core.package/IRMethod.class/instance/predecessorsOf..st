accessing
predecessorsOf: aSequence
	| predecessors |
	predecessors := OrderedCollection new.
	self allSequences do: [:each | (each successorSequences includes: aSequence) ifTrue: [predecessors add: each]].
	^predecessors