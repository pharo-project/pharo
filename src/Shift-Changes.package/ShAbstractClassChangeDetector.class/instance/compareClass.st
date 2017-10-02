comparing
compareClass
	oldValue := classAccessor value: builder oldClass.
	newValue := builderAccessor value: builder.

	^ (comparer value: oldValue value: newValue)
		ifTrue: [ #() ]
		ifFalse: [ self newChanges ]