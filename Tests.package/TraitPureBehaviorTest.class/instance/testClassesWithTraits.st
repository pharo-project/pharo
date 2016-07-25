testing-applying trait composition
testClassesWithTraits
	"self debug: #testClassesWithTraits"

	self assert: (self c1 methodDict includesKey: #foo).
	self assert: (self c2 methodDict includesKey: #bar).
	self assert: (self c2 methodDict includesKey: #m51).
	self assert: (self c2 methodDict includesKey: #m12).
	self assert: (self c2 methodDict includesKey: #m13).
	self assert: (self c2 methodDict includesKey: #m21).
	self assert: (self c2 methodDict includesKey: #m22).
	
	self deny: self c1 class hasTraitComposition.
	self assert: self c2 class hasTraitComposition.

	self assert: (self c2 class traitComposition size = 1).
	self assert: (self c2 class traitComposition includesTrait: self t5 classTrait)