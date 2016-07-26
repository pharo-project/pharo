testing
isBold
	styleFlags == nil ifTrue:[^false].
	^styleFlags allMask: StyleFlagBold