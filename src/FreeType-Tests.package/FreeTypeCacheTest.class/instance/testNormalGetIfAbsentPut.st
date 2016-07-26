tests
testNormalGetIfAbsentPut
	| u g r |
	cache maximumSize: nil. 
	u := cache instVarNamed: #used.
	r := cache 
		atFont: font1 
		charCode: $X asInteger 
		type: FreeTypeCacheGlyph 
		ifAbsentPut: [font1XGlyph].
	self assert: (r isKindOf: GlyphForm).
	self assert: (cache instVarNamed: #used) > u. "grown"
	self validateSizes: cache.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	self assert: g == font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache
	