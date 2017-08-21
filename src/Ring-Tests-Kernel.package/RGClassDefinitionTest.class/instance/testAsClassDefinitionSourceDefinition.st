testing
testAsClassDefinitionSourceDefinition

	| newClass |
	newClass:= MOPTestClassC asRingDefinition.
	self assert: (newClass definitionSource =  'Object subclass: #MOPTestClassC
	uses: Trait2
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Tests-Traits-MOP''').
	
	self assert: (newClass theMetaClass definitionSource =  'MOPTestClassC class
	uses: Trait2 classTrait
	instanceVariableNames: ''''').
	