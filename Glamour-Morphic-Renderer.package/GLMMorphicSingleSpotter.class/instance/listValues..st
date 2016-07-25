accessing-list
listValues: aCollection
	| previous |
	previous := self listSelection.
	listValues := aCollection asArray.
	listLabels := listValues
		collect: [ :each | model labelFor: each ].
	self changed: #listLabels; listSelection: previous