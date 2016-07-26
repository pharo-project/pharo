tests
testRemoveInstVarNamedClassInterface

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(x y z) 
		].
	aClass removeInstVarNamed: #y.

	self assert: aClass instVarNames = #(x z).