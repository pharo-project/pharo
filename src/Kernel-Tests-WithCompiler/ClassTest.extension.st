Extension { #name : #ClassTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
ClassTest >> testChangingShapeDoesNotPutNilInMethodsLastLiteralKey [
	"Test that when the shape of a class changes, the key of the last literal of the methods is not nil"
	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu compile: 'foo'.
	self deny: (tutu >> #foo) allLiterals last key isNil.
	tutu addInstVarNamed: 'x'.
	self deny: (tutu >> #foo) allLiterals last key isNil.
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ClassTest >> testCompileAll [
	ClassTest compileAll
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ClassTest >> testPoolVariableAccessibleInClassUser [
	"This test shows that a Pool Variable is accessible from the class that declare the Pool usage: here the superclass"
 
	PoolDefiner initialize.
	RootClassPoolUser compileAll.
	
	self assert: RootClassPoolUser gloups = 42.
	self assert: RootClassPoolUser author = 'Ducasse'
]

{ #category : #'*Kernel-Tests-WithCompiler' }
ClassTest >> testPoolVariableAccessibleInSubclassOfClassUser [
	"This test shows that a Pool Variable is not accessible from a subclass that declare the Pool usage: here SubFlop subclass of Flop and this is a bug. "
 
	PoolDefiner initialize.
	SubclassPoolUser compileAll.
	
	self assert: SubclassPoolUser gloups = 42.
	self assert: SubclassPoolUser author = 'Ducasse'
]
