testing
testAsClassDefinition2

	| newClass |
	newClass:= MOPTestClassC asRingDefinition.
	
	self assert: (newClass isRingObject).
	self assert: (newClass isClass).  
	self assert: (newClass name == #MOPTestClassC).  
	self assert: (newClass category notNil).
	self assert: (newClass superclassName notNil).
	self assert: (newClass traitCompositionSource = 'Trait2').
	
	self assert: (newClass theMetaClass isRingObject).
	self assert: (newClass theMetaClass isClass).
	self assert: (newClass theMetaClass traitCompositionSource = 'Trait2 classTrait').
	