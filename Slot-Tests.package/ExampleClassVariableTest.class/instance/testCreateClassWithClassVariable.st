tests
testCreateClassWithClassVariable
	"Add class variable using the builder interface"
	
	| classVar |
	
	classVar := #ClassVar => ExampleClassVariable.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {classVar}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar').


