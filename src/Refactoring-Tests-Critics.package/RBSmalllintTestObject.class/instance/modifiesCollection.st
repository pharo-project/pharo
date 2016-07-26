methods
modifiesCollection
	| collection |
	collection := (1 to: 10) asOrderedCollection.
	collection do: [:each | each > 5 ifTrue: [collection remove: each]].
	^collection