testing
testAsClassDefinitionSourceDefinition

	| newClass |
	newClass:= Trait asRingDefinition.
	self assert: (newClass definitionSource =  'TraitDescription subclass: #Trait
	uses: TClass
	instanceVariableNames: ''name environment classTrait category''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Traits-Kernel''').
	
	self assert: (newClass theMetaClass definitionSource =  'Trait class
	uses: TClass classTrait
	instanceVariableNames: ''''').
	