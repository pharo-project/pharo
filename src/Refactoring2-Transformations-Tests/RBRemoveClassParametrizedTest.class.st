Class {
	#name : #RBRemoveClassParametrizedTest,
	#superclass : #RBComposeParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBRemoveClassParametrizedTest class >> testParameters [

	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveClassRefactoring .
					  #constructor -> #classNames: .
					  #isComposed -> true};
		yourself
]

{ #category : #'failure tests' }
RBRemoveClassParametrizedTest >> testFailureRaisesRBRefactoringErrorWhenRemovingNonEmptySuperclass [

	| class |
	class := ('RBTransformation' , 'RuleTestData1') asSymbol.
	self shouldFail:
		(self createRefactoringWithModel: model andArguments: { class })
]

{ #category : #'failure tests' }
RBRemoveClassParametrizedTest >> testFailureRemoveClassWithBadNameRaisesRBRefactoringError [

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: #( #RecursiveSelfRule ))
]

{ #category : #'failure tests' }
RBRemoveClassParametrizedTest >> testFailureRemoveClassWithSubclasses [

	self shouldFail:
		(self createRefactoringWithArguments: { #RBFooLintRuleTestData1 })
]

{ #category : #tests }
RBRemoveClassParametrizedTest >> testRemoveAClassAndTheirSubclass [

	| aRefactoringColl |

	aRefactoringColl := self createRefactoringWithModel: model
		andArguments: {#RBSharedPoolForTestData2 . #RBSharedPoolForTestData1 }.

	self executeRefactorings: aRefactoringColl.

	self deny: (model includesClassNamed: #RBSharedPoolForTestData2).
	self deny: (model includesClassNamed: #RBSharedPoolForTestData1)
]

{ #category : #tests }
RBRemoveClassParametrizedTest >> testRemoveAClassAndTheirSubclass2UsingAlimitedEnvironment [
	"This test verifies that the preconditions do not escape the metamodel."

	| aRefactoringColl package |
	package := RBPackageEnvironment packageName:
		           RBFooDummyLintRuleTest1 packageName.
	model := RBNamespace onEnvironment: package.
	
	aRefactoringColl := self createRefactoringWithModel: model
		andArguments: {RBFooDummyLintRuleTest1 name . RBFooDummyLintRuleTest1 subclasses first name }.

	self executeRefactorings: aRefactoringColl.

	self deny: (model includesClassNamed: RBFooDummyLintRuleTest1 name).
	self deny: (model includesClassNamed: RBFooDummyLintRuleTest1 subclasses first name)
]

{ #category : #tests }
RBRemoveClassParametrizedTest >> testRemoveClassesWithReferencesBetweenThem [

	| aRefactoringColl |
	aRefactoringColl := self createRefactoringWithModel: model
		andArguments: { #RBSharedPoolForTestData2 . #RBRemoveClassParametrizedTest }.

	self executeRefactorings: aRefactoringColl.

	self deny: (model includesClassNamed: #RBRemoveClassParametrizedTest).
	self deny: (model includesClassNamed: #RBSharedPoolForTestData2)
]

{ #category : #tests }
RBRemoveClassParametrizedTest >> testRemoveEmptySuperclass [
	| aRefactoringColl classA classB classC |

	classA := ('RBFoo' , 'LintRuleTestData1') asSymbol.
	classB := ('RBTransformation' , 'RuleTestData1') asSymbol.
	classC := #RBLintRuleTestData.
	aRefactoringColl := self createRefactoringWithModel: model andArguments: {classA}.

	self executeRefactorings: aRefactoringColl.

	self deny: (model includesClassNamed: classA).
	self assert: (model classNamed: classB) superclass equals: (model classNamed: classC)
]

{ #category : #'failure tests' }
RBRemoveClassParametrizedTest >> testShouldWarnWhenRemovingClassWithReferences [

	self shouldWarn: (self
			 createRefactoringWithModel: model
			 andArguments: #( #RBBasicLintRuleTestData ))
]
