transforming
removeOldAccessors
	| oldAccessors |
	oldAccessors := (class allSelectors
		select: [ :each | each = variableName asSymbol or: [ each = (variableName , ':') asSymbol ] ]
		thenCollect: [ :each | class methodFor: each ]) asOrderedCollection.
	oldAccessors do: [ :each | class removeMethod: each selector ]