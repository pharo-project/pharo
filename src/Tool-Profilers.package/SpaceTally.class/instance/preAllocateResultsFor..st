class analysis
preAllocateResultsFor: classes

	results := OrderedCollection new: classes size.
	classes do: [:cl | results add: (SpaceTallyItem analyzedClassName: cl name)].
	results := results asArray.
