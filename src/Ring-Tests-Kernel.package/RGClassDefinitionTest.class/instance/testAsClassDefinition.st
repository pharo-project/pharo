testing
testAsClassDefinition
	| newClass |

	newClass:= OrderedCollection asRingDefinition.
	
	self assert: (newClass isRingObject).
	self assert: (newClass isClass).  
	self assert: (newClass name == #OrderedCollection).  
	self assert: (newClass category notNil).
	self assert: (newClass superclassName notNil).
	
	self assert: (newClass theMetaClass isRingObject).
	self assert: (newClass theMetaClass isClass).