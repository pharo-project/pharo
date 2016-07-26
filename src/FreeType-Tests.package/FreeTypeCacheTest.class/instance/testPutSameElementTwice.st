tests
testPutSameElementTwice
	| |
	cache maximumSize: nil. 
	cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph put: font1XGlyph.
	self assert: (cache instVarNamed: #used) = (cache sizeOf: font1XGlyph).
	self validateSizes: cache. 
	self validateCollections: cache.
	cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph put: font1XGlyph.
	self assert: (cache instVarNamed: #used) = (cache sizeOf: font1XGlyph).
	self validateSizes: cache. 
	self validateCollections: cache
	