tests
testFailedGet
	| |

	self 
		should: [cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheGlyph] 
		raise: Error.
	self assert: (cache instVarNamed: #fontTable) isEmpty. 
	self assert: (cache instVarNamed: #used) = 0.
	self validateSizes: cache
	