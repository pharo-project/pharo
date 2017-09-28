tests
testEmptyClass
	builder name: #ShCBEmptyClass.
	result := builder build.
	
	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #()
