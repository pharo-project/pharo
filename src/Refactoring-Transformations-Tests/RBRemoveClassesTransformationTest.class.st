Class {
	#name : 'RBRemoveClassesTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'accessing' }
RBRemoveClassesTransformationTest class >> defaultTimeLimit [
	^20 seconds
]

{ #category : 'tests' }
RBRemoveClassesTransformationTest >> testCanRemoveReferencedClass [

	| refactoring package |
	package := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           self class packageName }.
	"The second package has references to the leaf subclass we want to remove and this is part of the test."
	model := RBNamespace onEnvironment: package.
	refactoring := ReRemoveClassesTransformation
		               model: model
		               classNames: { RBTransformationDummyRuleTest1 name }.
	"Pay attention that we need a direct reference outside of the data for testing package: this is why we use RBTransformationDummyRuleTest1 name and not 'RBTransformationDummyRuleTest1'."

	"Yes we can remove a leaf subclass even if it is referenced"


	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert:
		(refactoring model classNamed: #RBTransformationDummyRuleTest1) isNil
	"In Pharo subclasses are removed now in the model this is unclear."
]

{ #category : 'tests' }
RBRemoveClassesTransformationTest >> testCanRemoveSuperclass [

	| refactoring package |
	package := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           self class packageName }.
	"The second package has reference to the leave subclass and this is part of the test."
	model := RBNamespace onEnvironment: package.
	refactoring := ReRemoveClassesTransformation
		               model: model
		               classNames: { RBFooDummyLintRuleTest1 name }.
	"Yes we can remove the superclass even if it has subclasses"

	"Pay attention that we need a direct reference outside of the data for testing package."

	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert:
		(refactoring model classNamed: #RBFooDummyLintRuleTest1) isNil
	"In Pharo subclasses are removed now in the model this is unclear."
]

{ #category : 'tests' }
RBRemoveClassesTransformationTest >> testCanRemoveUnreferencedClass [

	| refactoring package |
	package := RBPackageEnvironment packageName:
		           RBFooDummyLintRuleTest1 packageName.
	model := RBNamespace onEnvironment: package.
	refactoring := ReRemoveClassesTransformation
		               model: model
		               classNames: { 'RBUnusedRootClass' asSymbol }.
	"Pay attention that we do not want a direct reference outside of the data for testing package."

	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert:
		(refactoring model classNamed: 'RBUnusedRootClass' asSymbol)
			isNil
]
