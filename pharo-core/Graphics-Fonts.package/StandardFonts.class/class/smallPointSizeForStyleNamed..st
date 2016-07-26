styles
smallPointSizeForStyleNamed: aSymbol
	| all s idx |
	StrikeFont defaultFontKey.
	all := TextStyle pointSizesFor: StrikeFont defaultFontKey.
	s := self pointSizeForStyleNamed: aSymbol.
	idx := all indexOf: s ifAbsent: [^ s].
	^ idx > 1
			ifTrue: [all at: idx - 1]
			ifFalse: [all at: idx]