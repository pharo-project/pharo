Class {
	#name : 'RBAddMethodRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBAddMethodRefactoringTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testAddMethodOverrideInSubclassShouldWarn [

"
B >>shouldNotBeOverriddenByAdd ^ something
      C2 >>       <---- here we add a class
"


	| rbNamespace refact |
	rbNamespace := RBNamespace onEnvironment: (RBClassEnvironment classes:  {MyClassA . MyClassB2. MyClassC2}).
	rbClass := rbNamespace classNamed: #MyClassC2.
	"The refactoring will override a method of the superclass and it should not."

	refact := (RBAddMethodRefactoring
				 model: rbNamespace
				 sourceCode: 'shouldNotBeOverriddenByAdd ^1'
				 in: rbClass
				 withProtocol: #accessing).
	self shouldWarn: refact. 
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testAddMethodOverridenWithSuperCallShouldWarn [
	"A >> x ^ 33
   		B is empty  <---- we add a method here
     		C >> x ^ super x + 10 "
		
	| rbNamespace refact |
	rbNamespace := RBNamespace onEnvironment: (RBClassEnvironment classes:  {MyClassA . MyClassB2. MyClassC2}).
	rbClass := rbNamespace classNamed: #MyClassB2.
	"The refactoring will override a method of the superclass and it should not."

	refact := (RBAddMethodRefactoring
				 model: rbNamespace
				 sourceCode: 'x ^10000'
				 in: rbClass
				 withProtocol: #accessing).
	self shouldWarn: refact. 
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testWarnModelExistingSelector [

	self
		shouldWarn: (RBAddMethodRefactoring
				 model: model
				 sourceCode: 'classVarName1 ^super printString'
				 in: (model classNamed: #Bar)
				 withProtocol: #accessing);
		shouldWarn: (RBAddMethodRefactoring
				 model: model
				 sourceCode: 'printString ^super printString'
				 in: (model classNamed: #Bar)
				 withProtocol: #accessing)
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testWarnModelInheritedSelector [

	| refactoring |
	refactoring := RBAddMethodRefactoring
		               model: model
		               sourceCode: 'printString ^super printString'
		               in: (model classNamed: #Bar)
		               withProtocol: #accessing .
	self shouldWarn: refactoring
]

{ #category : 'failure tests' }
RBAddMethodRefactoringTest >> testWarnWhenSelectorAlreadyExists [

	self shouldWarn: (RBAddMethodRefactoring
			 sourceCode: 'printString ^super printString'
			 in: RBBasicLintRuleTestData
			 withProtocol: #accessing)
]
