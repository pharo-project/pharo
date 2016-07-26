tests
testNormalPutGetWidth
	| u g |
	cache maximumSize: nil. 
	u := cache instVarNamed: #used.
	cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheWidth put: 100.
	self assert: (cache instVarNamed: #used) > u. "grown"
	self validateSizes: cache. 
	g := cache atFont: font1 charCode: $X asInteger type: FreeTypeCacheWidth.
	self assert: g = 100.
	self validateSizes: cache. 
	self validateCollections: cache
	