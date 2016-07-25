tests
testSetMaximumSize
	cache maximumSize: 0.
	self assert: (cache instVarNamed: #maximumSize) = 0.
	cache maximumSize: 99999999999999999.
	self assert: (cache instVarNamed: #maximumSize) = 99999999999999999.
	cache maximumSize: nil.	"unbounded"
	self assert: (cache instVarNamed: #maximumSize) isNil.
	self validateSizes: cache.
	self validateCollections: cache