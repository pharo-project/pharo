tests
testMaximumSizeRespectedOnPut
	| |
	cache maximumSize: (cache sizeOf: font1XGlyph).
	cache 
		atFont: font1 
		charCode: $X asInteger 
		type: FreeTypeCacheGlyph 
		put: font1XGlyph.
	self validateSizes: cache.
	self validateCollections: cache.
	cache 
		atFont: font1 
		charCode: $Y asInteger 
		type: FreeTypeCacheGlyph 
		put: font1XGlyph.	
	self assert: (cache instVarNamed:#used) = 0. "cache has been cleared on reaching max size"
	self validateSizes: cache.
	self validateCollections: cache.
	self 
		should: [cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph]
		raise: Error.	
	self 
		should: [cache atFont: font1 charCode: $Y asInteger type: FreeTypeCacheGlyph]
		raise: Error.	
	