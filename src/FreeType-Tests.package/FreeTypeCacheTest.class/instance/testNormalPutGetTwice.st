tests
testNormalPutGetTwice
	| u g |
	cache maximumSize: nil. 
	u := cache instVarNamed: #used.
	cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph put: font1XGlyph.
	self assert: (cache instVarNamed: #used) > u. "grown"
	self validateSizes: cache. 
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph.
	self assert: g == font1XGlyph.
	self validateSizes: cache. 
	self validateCollections: cache
	