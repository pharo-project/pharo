Class {
	#name : 'RBRenameInstanceVariableParametrizedTest',
	#superclass : 'RBRenameVariableParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBRenameInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRenameInstanceVariableRefactoring .
					  #constructor -> #rename:to:in: .
					  #extraArgument -> #() };
		addCase: { #rbClass -> RBRenameVariableTransformation .
					  #constructor -> #rename:to:in:classVariable: .
					  #extraArgument -> #(false) };
		yourself
]

{ #category : 'failure tests' }
RBRenameInstanceVariableParametrizedTest >> testFailureAlreadyExistingName [

	self shouldFail: (self createRefactoringWithArguments:
			 { 'classBlock'. 'name'. #RBBasicLintRuleTestData })
]

{ #category : 'failure tests' }
RBRenameInstanceVariableParametrizedTest >> testFailureMetaclassFailure [

	self shouldFail: (self createRefactoringWithArguments: {
				 #RecursiveSelfRule.
				 #Foo.
				 RBTransformationRuleTestData class name })
]

{ #category : 'failure tests' }
RBRenameInstanceVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithArguments:
			 { 'foo'. 'newFoo'. #RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBRenameInstanceVariableParametrizedTest >> testRenameInstVar [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ 'classBlock' .
		'asdf' .
		#RBBasicLintRuleTestData}.

	self executeRefactoring: refactoring.

	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self deny: (class directlyDefinesInstanceVariable: 'classBlock').
	self
		assert:
			(class parseTreeForSelector: #checkClass:)
				equals:
					(self
						parseMethod:
							'checkClass: aSmalllintContext
								^asdf value: aSmalllintContext value: result').
	self
		assert:
			(class parseTreeForSelector: #initialize)
				equals:
					(self
						parseMethod:
							'initialize
	super initialize.
	self anInstVar: 1.
	asdf := [:context :aResult | ].
	methodBlock := [:context :aResult | ].
	self resultClass: RBSelectorEnvironment')
]

{ #category : 'tests' }
RBRenameInstanceVariableParametrizedTest >> testRenameInstVarFromTrait [
	| refactoring class |

	refactoring := self createRefactoringWithArguments:
		{ 'var1' .
		'var11' .
		#RBTDummy }.

	self executeRefactoring: refactoring.

	class := refactoring model classNamed: #RBTDummy.
	self assert: (class directlyDefinesInstanceVariable: 'var11').
	self deny: (class directlyDefinesInstanceVariable: 'var1').
	self
		assert: (class parseTreeForSelector: #var1)
		equals: (self parseMethod: 'var1 ^ var11').
	class := refactoring model classNamed: #RBClassUsingSharedPoolForTestData.
	self flag: 'TODO: for transformation'
	"self
		assert: (class parseTreeFor: #msg1)
		equals: (self parseMethod: 'msg1 var11 asString'). "
	"TODO"
	"self
		assert: (class parseTreeFor: #msg2)
		equals: (self parseMethod: 'msg2 var11')"
]

{ #category : 'tests' }
RBRenameInstanceVariableParametrizedTest >> testRenameInstVarNotAccessors [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ 'result' .
		'whatever' .
		#RBBasicLintRuleTestData }.

	self executeRefactoring: refactoring.

	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'whatever').
	self deny: (class directlyDefinesInstanceVariable: 'result').

	self deny: (class definesMethod: #whatever).
	self deny: (class definesMethod: #whatever:).
	self assert: (class definesMethod: #result)
]
