utilities
tokenTypesIn: aString
	| parser ranges |
	parser := SHParserST80 new.
	ranges := parser rangesIn: aString classOrMetaClass: Object workspace: nil environment: nil.
	^ranges asArray collect: [:each | each type]