Class {
	#name : #RBRemoveClassRefactoringTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #accessing }
RBRemoveClassRefactoringTest class >> defaultTimeLimit [
	^20 seconds
]

{ #category : #tests }
RBRemoveClassRefactoringTest >> testCanRemoveAReferencedClassNotHavingReferenceInTheModel [

	| refactoring package |
	package := RBPackageEnvironment packageName:
		           RBFooDummyLintRuleTest1 packageName.
	model := RBNamespace onEnvironment: package.
	"here the model only contains classes and there is no reference to RBTransformationDummyRuleTest1 and it is a subclass."
	
	refactoring := RBRemoveClassRefactoring
		               model: model
		               classNames: { RBTransformationDummyRuleTest1 name }.
	"We can use a direct reference here because we want to check in particular that THIS reference is not taken 
	into account by the preconditions (They should not escape the model by directly using Pharo metamodel."
	

	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert:
		(refactoring model classNamed: #RBTransformationDummyRuleTest1) isNil
]

{ #category : #'tests environment' }
RBRemoveClassRefactoringTest >> testCanRemoveUnreferencedClassUsingLimitedEnvironmentButReferencedElsewhere [

	| refactoring package |
	package := RBPackageEnvironment packageName: RBFooDummyLintRuleTest1 packageName.
	model := RBNamespace onEnvironment: package.
	"here the model only contains classes and there is no reference to RBTransformationDummyRuleTest1 and it is a subclass."
	refactoring := RBRemoveClassRefactoring 
							model: model 
							classNames: { RBTransformationDummyRuleTest1 name }.
	"Pay attention that we need a direct reference outside of the data for testing package."

	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert:
		(refactoring model classNamed: #RBTransformationDummyRuleTest1) isNil
]

{ #category : #'tests environment' }
RBRemoveClassRefactoringTest >> testCanRemoveUnreferencedClassWithFullEnvironment [

	| refactoring |
	model := RBNamespace onEnvironment: RBBrowserEnvironment new .
	refactoring := RBRemoveClassRefactoring
		               model: model
		               classNames: { 'RBUnusedRootClass' asSymbol }.
	"Pay attention that we do not want to create a direct reference."

	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert: (refactoring model classNamed: 'RBUnusedRootClass' asSymbol) isNil
]

{ #category : #tests }
RBRemoveClassRefactoringTest >> testPreconditionHasNoReferences [

	| refactoring  package sup sub |
   package := RBPackageEnvironment packageName: RBFooDummyLintRuleTest1 packageName.
	model := RBNamespace onEnvironment: package.
	refactoring := RBRemoveClassRefactoring 
							model: model 
							classNames: { #RBFooDummyLintRuleTest1 }.
	sup := (refactoring model classNamed: #RBFooDummyLintRuleTest1).
	sub := (refactoring model classNamed: RBFooDummyLintRuleTest1 subclasses first name).
	self assert: sup isNotNil.
	self assert: sub isNotNil.
	
	"the class is a superclass so it should not be removed."
	self deny: refactoring preconditions check.
	
	self deny: (refactoring preconditionEmptyOrHasNoSubclasses: sup) check.
	

	self assert:
		(refactoring model classNamed: #RBFooDummyLintRuleTest1) isNotNil
]

{ #category : #'tests preconditions' }
RBRemoveClassRefactoringTest >> testPreconditionHasNoReferencesWithClassesWithReferencesBetweenThem [

	| refact classes |
	classes := RBClassEnvironment classes: { RBSharedPoolForTestData2 . RBRemoveClassParametrizedTest}.
	model := RBNamespace onEnvironment: classes.
	
	refact := RBRemoveClassRefactoring 
					model: model 
					classNames: { #RBSharedPoolForTestData2 . #RBRemoveClassParametrizedTest }.
	"RBSharedPoolForTestData2 refers to RBRemoveClassParametrizedTest and nobody else refers to them!"
	
	self assert: (refact preconditionHasNoReferences: (refact model classNamed: #RBSharedPoolForTestData2)) check.
]

{ #category : #'tests preconditions' }
RBRemoveClassRefactoringTest >> testPreconditionHasNoSymbolUseHandlesCorrectlyRemovedClasses [

	| refact classes |
	classes := RBClassEnvironment classes: { OnlyReferencing . OnlyReferencedByASymbol}.
	"since the model is only these classes, such references are not introducing extra references for the test"
	model := RBNamespace onEnvironment: classes.
	
	refact := RBRemoveClassRefactoring 
					model: model 
					classNames: { #OnlyReferencing . #OnlyReferencedByASymbol }.
		
	self assert: (refact preconditionHasNoUsers: (refact model classNamed: #OnlyReferencedByASymbol)) check.
	"It should be true because uses created by classes to be removed should not be counted."
	
]

{ #category : #'tests preconditions' }
RBRemoveClassRefactoringTest >> testPreconditionHasNoSymbolUseShouldFail [

	| refact classes |
	classes := RBClassEnvironment classes: { OnlyReferencing . OnlyReferencedByASymbol}.
	"since the model is only these classes, such references are not introducing extra references for the test"
	model := RBNamespace onEnvironment: classes.
	
	refact := RBRemoveClassRefactoring 
					model: model 
					classNames: { #OnlyReferencedByASymbol }.
		
	self deny: (refact preconditionHasNoUsers: (refact model classNamed: #OnlyReferencedByASymbol)) check.
	"It should be false because the referencing class is not removed and is part of the model"
	
]

{ #category : #tests }
RBRemoveClassRefactoringTest >> testRemovingASuperclassShouldNotRemoveTheClass [

	| refactoring  package sup sub |
   package := RBPackageEnvironment packageName: RBFooDummyLintRuleTest1 packageName.
	model := RBNamespace onEnvironment: package.
	refactoring := RBRemoveClassRefactoring 
							model: model 
							classNames: { #RBFooDummyLintRuleTest1}.
	sup := (refactoring model classNamed: #RBFooDummyLintRuleTest1).
	sub := (refactoring model classNamed: RBFooDummyLintRuleTest1 subclasses first name).
	self assert: sup isNotNil.
	self assert: sub isNotNil.
	
	"the class is a superclass so it should not be removed."
	self deny: refactoring preconditions check.
	
	"but for the real reason"
	
	self deny: (refactoring preconditionEmptyOrHasNoSubclasses: sup) check.
	
	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert:
		(refactoring model classNamed: #RBFooDummyLintRuleTest1) isNotNil
]
