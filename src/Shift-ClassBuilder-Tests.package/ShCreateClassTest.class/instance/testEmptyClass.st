tests
testEmptyClass
	builder name: #SHTestEmptyClass.
	result := builder build.
	
	self validateResult.
	self validateSuperclass: Object.
