tests
testMigrateClassVar
	"Add class variable using the builder interface and change it to another kind"
	
	| classVar |
	
	classVar := #ClassVar => ExampleClassVariable.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {#ClassVar}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar').
	self assert: (aClass classVariableNamed: #ClassVar) class = ClassVariable.
	
	aClass classVarNamed: 'ClassVar' put: 5.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {classVar}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar').
	self assert: (aClass classVariableNamed: #ClassVar) class = ExampleClassVariable.
	self assert: (aClass classVariableNamed: #ClassVar) read = 5.