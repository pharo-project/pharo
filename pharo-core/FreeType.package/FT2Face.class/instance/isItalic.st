testing
isItalic
	styleFlags == nil ifTrue:[^false].
	^styleFlags allMask: StyleFlagItalic