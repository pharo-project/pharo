Class {
	#name : #RBRemoveClassTransformationTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #accessing }
RBRemoveClassTransformationTest class >> defaultTimeLimit [
	^20 seconds
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testCanRemoveReferencedClass [

	| refactoring package |
	package := RBPackageEnvironment packageNames: { RBFooDummyLintRuleTest1 packageName . self class packageName }.
	model := RBNamespace onEnvironment: package.
	refactoring := RBRemoveClassTransformation 
							model: model 
							classNames: {RBFooDummyLintRuleTest1 name}.
	"Yes we can remove the superclass even if it has subclasses"
							
	"Pay attention that we need a direct reference outside of the data for testing package."

	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert: (refactoring model classNamed: #RBFooDummyLintRuleTest1) isNil.
	"In Pharo subclasses are removed now in the model this is unclear."
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testCanRemoveUnreferencedClass [

	| refactoring package |
	package := RBPackageEnvironment packageName: RBFooDummyLintRuleTest1 packageName.
	model := RBNamespace onEnvironment: package.
	refactoring := RBRemoveClassTransformation 
							model: model 
							classNames: {RBTransformationDummyRuleTest1 name}.
	"Pay attention that we need a direct reference outside of the data for testing package."

	self
		shouldnt: [ refactoring generateChanges ]
		raise: RBRefactoringError.

	self assert:
		(refactoring model classNamed: #RBTransformationDummyRuleTest1) isNil
]
