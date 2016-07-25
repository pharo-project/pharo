asserting
assertButtonExists: aString
	self buttonMorphs detect: [:m | m label = aString] ifNone: [self assert: false].
				