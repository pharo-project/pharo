tests - class creation
testNewSubclass
	| cls |
	cls := Point newSubclass.
	self assert: (cls isBehavior).
	self assert: (cls superclass == Point).
	self assert: (Point allSubclasses includes: cls).
	self assert: (cls instVarNames = #()).
	self assert: (cls category = self unclassifiedCategory).
	self assert: (cls classVarNames = #()).
	
	cls removeFromSystem.