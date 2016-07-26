tests
testCreateClassWithTwoClassVariable
	"Add class variable using the builder interface"
	
	| classVar1 classVar2 |
	
	classVar1 := #ClassVar1 => ExampleClassVariable.
	classVar2 := #ClassVar2 => ExampleClassVariable.
	
	aClass := self make: [ :builder |
		builder 
			sharedVariables: {classVar1. classVar2}
		].
	
	self assert: (aClass hasClassVarNamed: 'ClassVar1').
	self assert: (aClass hasClassVarNamed: 'ClassVar2').


