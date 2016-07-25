testing
testAsClassDefinition2

	| newClass |
	newClass:= Trait asRingDefinition.
	
	self assert: (newClass isRingObject).
	self assert: (newClass isClass).  
	self assert: (newClass name == #Trait).  
	self assert: (newClass category notNil).
	self assert: (newClass superclassName notNil).
	self assert: (newClass  traitCompositionSource = 'TClass').
	
	self assert: (newClass theMetaClass isRingObject).
	self assert: (newClass theMetaClass isClass).
	self assert: (newClass theMetaClass traitCompositionSource = 'TClass classTrait').
	