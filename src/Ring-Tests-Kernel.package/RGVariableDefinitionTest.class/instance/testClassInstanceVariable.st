testing
testClassInstanceVariable
	| instVar newClass metaClass |
	
	instVar:= RGClassInstanceVariableDefinition named: 'sizes'.
	
	self assert: (instVar isClassInstanceVariable).
	self assert: (instVar isVariable).
	self assert: (instVar name = 'sizes').
	self assert: (instVar parent isNil).
	self assert: (instVar isMetaSide).
	
	newClass:= RGClassDefinition named: #OrderedCollection.
	newClass withMetaclass.
	metaClass:= newClass theMetaClass.
	instVar:= metaClass addInstVarNamed: 'sizes'.
	self assert: (instVar parent = metaClass).	
	self assert: (metaClass instanceVariables size = 1).