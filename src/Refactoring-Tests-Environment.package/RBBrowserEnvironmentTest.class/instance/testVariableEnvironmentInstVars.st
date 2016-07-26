testing
testVariableEnvironmentInstVars
	| refs writers readers classRefs |
	refs := RBVariableEnvironment
		referencesToInstanceVariable: 'universalEnvironment'
		in: self class.
	writers := RBVariableEnvironment
		writersOfInstanceVariable: 'universalEnvironment'
		in: self class.
	readers := RBVariableEnvironment
		readersOfInstanceVariable: 'universalEnvironment'
		in: self class.
	classRefs := RBVariableEnvironment
		referencesToClassVariable: 'ClassVariable'
		in: self class.
	self universalTestFor: refs.
	self universalTestFor: writers.
	self universalTestFor: readers.
	self universalTestFor: classRefs.
	self deny: universalEnvironment isVariableEnvironment.
	self assert: refs isVariableEnvironment.
	self assert: refs numberSelectors = (writers | readers) numberSelectors