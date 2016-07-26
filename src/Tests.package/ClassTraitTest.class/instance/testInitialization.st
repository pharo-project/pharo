testing
testInitialization
	"self run: #testInitialization"
	| classTrait |
	classTrait := self t1 classTrait.
	self assert: self t1 hasClassSide.
	self assert: self t1 classTrait == classTrait.
	self assert: classTrait isClassTrait.
	self assert: classTrait classSide == classTrait.
	self deny: classTrait isBaseTrait.
	self assert: classTrait baseTrait == self t1.

	"assert classtrait methods are propagated to users when setting traitComposition"
	self assert: self t4 hasClassSide.
	self assert: self t5 hasClassSide.
	self assert: (self t2 classSide includesLocalSelector: #m2ClassSide:).
	self assert: (self t4 classSide includesSelector: #m2ClassSide:).
	self assert: (self t5 classSide includesSelector: #m2ClassSide:).
	self assert: (self c2 m2ClassSide: 17) = 17