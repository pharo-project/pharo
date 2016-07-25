testing
testAsMetaclassDefinition
	| rgClass class |
	class := Class class.
	rgClass:= class asRingDefinition.
	
	self assert: (rgClass isRingObject).
	self assert: (rgClass isClass).  
	self assert: (rgClass name == #'Class class').  
	self assert: (rgClass category == class category).
	self assert: (rgClass theMetaClass == rgClass).
