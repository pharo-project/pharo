tests-basic
testBasicClassBuilding

	aClass := self make: [ :builder |
		builder name: self aClassName ].
	
	self deny: aClass classLayout isNil.
	self assert: aClass name = self aClassName.
	self assert: (self class environment at: self aClassName) == aClass.
