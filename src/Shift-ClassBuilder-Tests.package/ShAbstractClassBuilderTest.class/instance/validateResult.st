validation
validateResult
	| obj |
	self assert: result isClass.
	self assert: result class isMeta.
	
	obj := result new.
	self assert: obj isNotNil.
	self assert: obj class == result.
