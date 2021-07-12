Class {
	#name : #RBTemporaryToInstanceVariableParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBTemporaryToInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBTemporaryToInstanceVariableRefactoring };
		yourself
]

{ #category : #running }
RBTemporaryToInstanceVariableParametrizedTest >> addClassHierarchy [
	model 
		defineClass: 'Foo subclass: #Foo1 instanceVariableNames: ''foo'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Foo)
		compile: 'someMethod | foo | foo := 4. ^foo'
		classified: #(#accessing).
]

{ #category : #running }
RBTemporaryToInstanceVariableParametrizedTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #'failure tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testHierarchyDefinesVarableNamedAsTemporary [
	| class |
	self addClassHierarchy.
	class := model classNamed: #Foo.
	self shouldWarn: (rbClass 
				class: class
				selector: #someMethod
				variable: 'foo')
]

{ #category : #'failure tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testNonExistantName [
	self
		shouldFail: (rbClass 
				class: RBBasicLintRuleTestData
				selector: #checkClass:
				variable: 'asdf');
		shouldFail: (rbClass 
				class: RBBasicLintRuleTestData
				selector: #checkClass1:
				variable: 'aSmalllintContext')
]

{ #category : #'failure tests' }
RBTemporaryToInstanceVariableParametrizedTest >> testRedefinedTemporary [
	| class |
	class := model classNamed: #Foo.
	class 
		compile: 'someMethod | instVarName1 | instVarName1 := 4. ^instVarName1'
		classified: #(#accessing).
	self shouldFail: (rbClass 
				class: class
				selector: #someMethod
				variable: 'instVarName1')
]

{ #category : #tests }
RBTemporaryToInstanceVariableParametrizedTest >> testTemporaryToInstanceVariable [
	| refactoring class |
	refactoring := rbClass 
		class: RBLintRuleTestData
		selector: #displayName
		variable: 'nameStream'.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeFor: #displayName) equals: (self parseMethod: 'displayName
								nameStream := WriteStream on: (String new: 64).
								nameStream
									nextPutAll: self name;
									nextPutAll: '' (''.
								self problemCount printOn: nameStream.
								nameStream nextPut: $).
								^nameStream contents').
	self assert: (class directlyDefinesInstanceVariable: 'nameStream')
]

{ #category : #tests }
RBTemporaryToInstanceVariableParametrizedTest >> testWhenHierarchyDefinesVariableNamedAsTemporary [
	| class refactoring |
	self addClassHierarchy.
	class := model classNamed: #Foo.
	refactoring := rbClass 
				class: class
				selector: #someMethod
				variable: 'foo'.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self assert: (class definesInstanceVariable: 'foo').
	self deny: ((model classNamed: #Foo1) directlyDefinesInstanceVariable: 'foo').
	self assert: (class parseTreeFor: #someMethod) equals: (self parseMethod: 'someMethod foo := 4. ^ foo'). 
	
]
