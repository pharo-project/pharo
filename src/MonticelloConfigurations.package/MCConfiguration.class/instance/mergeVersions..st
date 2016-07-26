private
mergeVersions: aCollection

	| merger |
	aCollection isEmpty ifTrue: [^0].
	merger := MCVersionMerger new.
	aCollection do: [:each | merger addVersion: each].
	merger mergeWithNameLike: self nameForChangeset.
	^ aCollection size