styles
bigPointSizeForStyleNamed: aSymbol
	| all s idx |
	StrikeFont defaultFontKey.
	all := TextStyle pointSizesFor: StrikeFont defaultFontKey.
	s := self pointSizeForStyleNamed: aSymbol.
	idx := all indexOf: s ifAbsent: [^ s].
	^ idx = all size
			ifTrue: [all at: idx]
			ifFalse: [all at: idx + 1]