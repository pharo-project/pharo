Class {
	#name : 'RBTemporaryToInstanceVariableParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBTemporaryToInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBTemporaryToInstanceVariableRefactoring };
		addCase: { #rbClass -> RBTemporaryToInstanceVariableTransformation };
		yourself
]

{ #category : 'running' }
RBTemporaryToInstanceVariableParametrizedTest >> addClassHierarchy [

	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #Foo;
			name: #Foo1;
			slots: { #foo };
			package: #'Refactory-Test data' ].
	(model classNamed: #Foo) compile: 'someMethod | foo | foo := 4. ^foo' classified: #( #accessing )
]

{ #category : 'accessing' }
RBTemporaryToInstanceVariableParametrizedTest >> constructor [
	^ #class:selector:variable:
]

{ #category : 'running' }
RBTemporaryToInstanceVariableParametrizedTest >> setUp [
	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'failure tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testFailureHierarchyDefinesVarableNamedAsTemporary [

	| class |
	self addClassHierarchy.
	class := model classNamed: #Foo.
	self shouldWarn: (self createRefactoringWithArguments: {
				 class.
				 #someMethod.
				 'foo' })
]

{ #category : 'failure tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithArguments: {
				 RBBasicLintRuleTestData.
				 #checkClass:.
				 'asdf' }).
	self shouldFail: (self createRefactoringWithArguments: {
				 RBBasicLintRuleTestData.
				 #checkClass1:.
				 'aSmalllintContext' })
]

{ #category : 'failure tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testFailureRedefinedTemporary [

	| class |
	class := model classNamed: #Foo.
	class
		compile:
		'someMethod | instVarName1 | instVarName1 := 4. ^instVarName1'
		classified: #( #accessing ).
	self shouldFail: (self createRefactoringWithArguments: {
				 class.
				 #someMethod.
				 'instVarName1' })
]

{ #category : 'tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testTemporaryToInstanceVariable [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ RBLintRuleTestData . #displayName . 'nameStream'}.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeForSelector: #displayName) equals: (self parseMethod: 'displayName
								nameStream := WriteStream on: (String new: 64).
								nameStream
									nextPutAll: self name;
									nextPutAll: '' (''.
								self problemCount printOn: nameStream.
								nameStream nextPut: $).
								^nameStream contents').
	self assert: (class directlyDefinesInstanceVariable: 'nameStream')
]

{ #category : 'tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testWhenHierarchyDefinesVariableNamedAsTemporary [
	| class refactoring |
	self addClassHierarchy.
	class := model classNamed: #Foo.
	refactoring := self createRefactoringWithArguments:
		{ class . #someMethod . 'foo' }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self assert: (class definesInstanceVariable: 'foo').
	self deny: ((model classNamed: #Foo1) directlyDefinesInstanceVariable: 'foo').
	self assert: (class parseTreeForSelector: #someMethod) equals: (self parseMethod: 'someMethod foo := 4. ^ foo')
]
