merging
mergeSpec: anotherSpec

	| newSpec val |
	newSpec := super mergeSpec: anotherSpec.
	newSpec list: self list copy.
	anotherSpec list do: [:groupMember | groupMember applyToList: newSpec ].
	^newSpec