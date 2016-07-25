selection change
selectMoreAtTop
	| first |
	autoTargetMorph ifNil: [^self].
	first := autoTargetMorph index.
	first > 1
		ifTrue: [
			self autoMultiSelect: (self allNodeMorphs at: first - 1).
			self selectionChanged]