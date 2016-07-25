tests-valid extensions
testPointerWithVariableExtensionIsNowVariable
	"This test case reproduces a real bug."

	| superclass subclass |
	superclass := self make: [ :builder | 
		builder 
			superclass: Object;
			name: self aClassName ].

	subclass := self make: [ :builder | 
		builder 
			superclass: superclass;
			layoutClass: VariableLayout;
			name: self anotherClassName ].

	superclass := self make: [ :builder | 
		builder layoutClass: VariableLayout ].

	self assert: superclass isVariable.
	self assert: subclass isVariable.