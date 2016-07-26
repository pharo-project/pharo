instance creation
newWith: aRelationGraph named: aString

	| generator |
	generator := self new.
	generator relationGraph: aRelationGraph.
	generator nameOfGraph: aString.
	generator buildGraphViz.
	^ generator