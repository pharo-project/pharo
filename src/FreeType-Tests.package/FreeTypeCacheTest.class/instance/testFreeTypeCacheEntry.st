tests
testFreeTypeCacheEntry
	| f f2  f3 |
	f := FreeTypeCacheEntry new.
	f charCode: 1; font: font1; type: FreeTypeCacheGlyph; object: font1XGlyph.
	f2 := FreeTypeCacheEntry new.
	f2 charCode: 2; font: font1; type: FreeTypeCacheGlyphLCD; object: font1XGlyph.
	f nextLink: f2.
	self assert: f ~= f2.
	self assert: f nextLink = f2 .
	
	f3 := f copy.
	f3 nextLink: nil.
	self assert: f = f3. "equality not based on nextLink"