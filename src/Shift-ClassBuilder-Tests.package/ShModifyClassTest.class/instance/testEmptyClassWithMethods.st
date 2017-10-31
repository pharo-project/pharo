tests
testEmptyClassWithMethods
	builder name: #ShCBEmptyClassWithMethods.
	result := builder build.
	
	self validateResult.
	self validateSuperclass: Object.
	self validateMethods: #(aMethod anotherMethod:)
