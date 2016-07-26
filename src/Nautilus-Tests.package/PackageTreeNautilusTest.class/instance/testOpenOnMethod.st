tests
testOpenOnMethod
	nautilus := Nautilus openOnMethod: compiledMethod.
	self assertSelectedCompiledMethod.