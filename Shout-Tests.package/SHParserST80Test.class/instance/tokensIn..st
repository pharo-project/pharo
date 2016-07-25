utilities
tokensIn: aString
	| parser ranges |
	parser := SHParserST80 new.
	ranges := parser rangesIn: aString classOrMetaClass: Object workspace: nil environment: nil.
	^ranges asArray collect: [:each | aString copyFrom: each start to: each end]