tests
testEntriesRemovedFIFO
	| |
	cache maximumSize: 10*(cache sizeOf: font1XGlyph).
	1 to: 10 do:[:i |
		cache 
			atFont: font1 
			charCode: (1000-i) 
			type: FreeTypeCacheGlyph 
			put: font1XGlyph].
	self validateCollections: cache.
	11 to:1000 do:[:i |
		cache 
			atFont: font1 
			charCode: (1000-i) 
			type: FreeTypeCacheGlyph 
			put: font1XGlyph.
		self validateSizes: cache.
		self validateCollections: cache.
		"i-9 to: i do:[:i2 |
			self 
				shouldnt: [cache atFont: font1 charCode: 1000-i2 type: FreeTypeCacheGlyph]
				raise: Error]."
		self 
			should: [cache atFont: font1 charCode: 1000-(i-10) type: FreeTypeCacheGlyph]
			raise: Error].  
	self validateSizes: cache.

	