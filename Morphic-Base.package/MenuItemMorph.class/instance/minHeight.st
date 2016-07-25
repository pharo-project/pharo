layout
minHeight
	| iconHeight |
	iconHeight := self hasIcon
				ifTrue: [self icon height + 2]
				ifFalse: [0].
	^ self fontToUse height max: iconHeight 