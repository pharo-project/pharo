testing
testVariableEnvironmentClassVars
"test variable environment for class vars, refered on the class side only"
	| classRefs |
	classRefs := RBVariableEnvironment
		referencesToClassVariable: 'ClassSideOnlyVariable'
		in: self class.
	self universalTestFor: classRefs.
	self assert: classRefs methods isEmpty not.
