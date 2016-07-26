private
loadVersions: aCollection

	| loader |
	aCollection isEmpty ifTrue: [^0].
	loader := MCVersionLoader new.
	aCollection do: [:each | loader addVersion: each].
	loader loadWithNameLike: self nameForChangeset.
	^ aCollection size