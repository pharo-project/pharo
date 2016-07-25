selection change
selectMoreAtBottom
	| last |
	autoTargetMorph ifNil: [^self].
	last := autoTargetMorph index.
	last < self allNodeMorphs size
		ifTrue: [
			self autoMultiSelect: (self allNodeMorphs at: last + 1).
			self selectionChanged]