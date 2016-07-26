querying
retainedNodesFromList: aListOfNodes
	| retained |
	retained := OrderedCollection new.
	aListOfNodes do: [:n | [n item precondition value ifTrue: [retained add: n]] on: Error do: [retained add: n]].
	^ retained