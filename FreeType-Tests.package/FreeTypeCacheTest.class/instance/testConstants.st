tests
testConstants
	
	| constants |
	constants := {FreeTypeCacheWidth. FreeTypeCacheGlyphMono. FreeTypeCacheGlyphLCD.FreeTypeCacheGlyph}.
	self assert: constants asSet size = constants size. "no 2 have same value"
	self assert: (constants detect:[:x | x isNil] ifNone:[]) isNil. "no value is nil"
	
	