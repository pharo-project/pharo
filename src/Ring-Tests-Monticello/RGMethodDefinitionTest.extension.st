Extension { #name : #RGMethodDefinitionTest }

{ #category : #'*Ring-Tests-Monticello' }
RGMethodDefinitionTest >> testAsFullMethodDefinition [
	
	| rgMethod |
	rgMethod := (Class >> #asRingDefinition) asRingDefinition.
	self assert: rgMethod parent isNil.
	self assert: rgMethod package isNil.
	
	"retrieves package information from protocol name (class extension)"
	rgMethod := (Class >> #asRingDefinition) asFullRingDefinition.
	self assert: rgMethod parent notNil.
	self assert: rgMethod parentName = #Class.
	self assert: rgMethod parent methods size = 1.
	self assert: rgMethod parent package name equals: #Kernel.	
	self assert: rgMethod package notNil.
	self assert: rgMethod package name = #'Ring-Core-Kernel'.

	"retrieves package information from class package name "
	rgMethod := (Class >> #name) asFullRingDefinition.
	self assert: rgMethod parent notNil.
	self assert: rgMethod parentName = #Class.
	self assert: rgMethod parent methods size = 1.
	self assert: rgMethod parent package name equals: #Kernel.	
	self assert: rgMethod package notNil.
	self assert: rgMethod package name = #Kernel.
	
]
