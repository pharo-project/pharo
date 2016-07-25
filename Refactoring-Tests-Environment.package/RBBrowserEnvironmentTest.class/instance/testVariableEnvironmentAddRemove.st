testing
testVariableEnvironmentAddRemove
	| refs |
	refs := RBVariableEnvironment new.
	refs addClass: self class instanceVariable: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class instanceVariable: 'universalEnvironment'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0.
	refs addClass: self class instanceVariableReader: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class instanceVariableReader: 'universalEnvironment'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0.
	refs addClass: self class instanceVariableWriter: 'universalEnvironment'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class instanceVariableWriter: 'universalEnvironment'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0.
	refs addClass: self class classVariable: 'ClassVariable'.
	self assert: refs numberSelectors > 0.
	self assert: refs numberClasses = 1.
	refs removeClass: self class classVariable: 'ClassVariable'.
	self assert: refs numberSelectors = 0.
	self assert: refs numberClasses = 0
	