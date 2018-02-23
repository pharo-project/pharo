Extension { #name : #RGMethodDefinitionTest }

{ #category : #'*Ring-Tests-Monticello' }
RGMethodDefinitionTest >> testAsFullMethodDefinition [
	
	| rgMethod |
	rgMethod := (Trait >> #asRingDefinition) asRingDefinition.
	self assert: rgMethod parent isNil.
	self assert: rgMethod package isNil.
	
	"retrieves package information from protocol name (class extension)"
	rgMethod := (Trait >> #asRingDefinition) asFullRingDefinition.
	self assert: rgMethod parent notNil.
	self assert: rgMethod parentName = #Trait.
	self assert: rgMethod parent methods size = 1.
	self assert: rgMethod parent package name equals: #Traits.	
	self assert: rgMethod package notNil.
	self assert: rgMethod package name = #'Ring-Core-Kernel'.

	"retrieves package information from class package name "
	rgMethod := (Trait >> #name) asFullRingDefinition.
	self assert: rgMethod parent notNil.
	self assert: rgMethod parentName = #Trait.
	self assert: rgMethod parent methods size = 1.
	self assert: rgMethod parent package name equals: #Traits.	
	self assert: rgMethod package notNil.
	self assert: rgMethod package name = #Traits.
	
]
