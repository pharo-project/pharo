Class {
	#name : 'RBAbstractInstanceVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBAbstractInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAbstractInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBRemoveDirectAccessToVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : 'running' }
RBAbstractInstanceVariableParametrizedTest >> setUp [
	super setUp.
	model := self modelOnClasses: { Object . RBLintRuleTestData . RBBasicLintRuleTestData  }.
	self defineFooBarIn: model
]

{ #category : 'tests' }
RBAbstractInstanceVariableParametrizedTest >> testAbstractInstanceVariable [
	| refactoring class |
	refactoring := self createRefactoringWithModel: model andArguments:
		{ 'name' . #RBLintRuleTestData}.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeForSelector: #name) equals: (self parseMethod: 'name ^name').
	self assert: (class parseTreeForSelector: #name:) equals: (self parseMethod: 'name: aString
	name := aString').
	self assert: (class parseTreeForSelector: #initialize) equals: (self parseMethod: 'initialize

	<ignoreUnusedVariables: #( #foo1 )>
	<ignoreUnusedClassVariables: #( Foo1 )>
	super initialize.
	self name: ''''').
	self assert: (class parseTreeForSelector: #printOn:) equals: (self parseMethod: 'printOn: aStream
	self name ifNil: [ super printOn: aStream ] ifNotNil: [ aStream nextPutAll: self name ]')
]

{ #category : 'tests' }
RBAbstractInstanceVariableParametrizedTest >> testAbstractWithAssignmentUsed [
	| refactoring class |
	class := model classNamed: #Foo.
	refactoring := self createRefactoringWithModel:  model
				andArguments: { 'instVarName2' . #Foo}.
	self flag: 'Transformation has another result, reusing existing accessors'.
	self executeRefactoring: refactoring.
"	self assert: (class parseTreeFor: #foo)
		equals: (self parseMethod: 'foo
						^self instVarName21: 3').
	self
		assert: (class parseTreeFor: #instVarName2:) equals: (self
						parseMethod: 'instVarName2: anObject
						self instVarName21: anObject').
	self
		assert: (class parseTreeFor: #instVarName21:) equals: (self
						parseMethod: 'instVarName21: anObject
						^instVarName2 := anObject').
	self assert: (class parseTreeFor: #instVarName2)
				equals: (self parseMethod: 'instVarName2
						^instVarName2').
	self
		assert: ((model classNamed: #Bar) parseTreeFor: #foo) equals: (self
						parseMethod: 'foo
						instVarName1 := instVarName1 + self instVarName2 + ClassVarName1')"
]

{ #category : 'tests' }
RBAbstractInstanceVariableParametrizedTest >> testAbstractWithDefaultNamesUsed [
	| refactoring class |
	class := model classNamed: #Foo.
	refactoring := self createRefactoringWithModel: model andArguments:
				{'instVarName1' . #Foo}.
	self flag: 'There is an error using RBClass2 with pools'.
	"self executeRefactoring: refactoring.
	self
		assert: (class parseTreeFor: #bar) equals: (self
						parseMethod: 'bar
						""Add one to instVarName1""

						self instVarName11: self instVarName11 + 1').
	self
		assert: (class parseTreeFor: #instVarName11:) equals: (self
						parseMethod: 'instVarName11: anObject
						instVarName1 := anObject').
	self assert: (class parseTreeFor: #instVarName11)
			equals: (self parseMethod: 'instVarName11
						^instVarName1').
	self
		assert: ((model classNamed: #Bar) parseTreeFor: #foo) equals: (self
						parseMethod: 'foo
						self instVarName11: self instVarName11 + instVarName2 + ClassVarName1')"
]

{ #category : 'failure tests' }
RBAbstractInstanceVariableParametrizedTest >> testFailureInheritedName [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'name'. #RBBasicLintRuleTestData })
]

{ #category : 'failure tests' }
RBAbstractInstanceVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'foo'. #RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBAbstractInstanceVariableParametrizedTest >> testMetaclassInstanceVariables [
	| refactoring class |
	class := model metaclassNamed: #Foo.
	class addInstanceVariable: 'foo'.
	class compile: 'zzz ^foo := foo + foo * 2' classified: #(#testing).
	refactoring := self createRefactoringWithModel: model andArguments:
				{ 'foo' . #'Foo class'}.
	self flag: 'There is a transformation error'.
"	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo1)
		  equals: (self parseMethod: 'foo1
						^foo').
	self assert: (class parseTreeFor: #foo:)
		  equals: (self parseMethod: 'foo: anObject
						^foo := anObject').
	self assert: (class parseTreeFor: #zzz)
		  equals: (self parseMethod: 'zzz ^self foo: self foo1 + self foo1 * 2')"
]
