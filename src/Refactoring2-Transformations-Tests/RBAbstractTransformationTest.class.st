Class {
	#name : #RBAbstractTransformationTest,
	#superclass : #RBAbstractEnvTestCase,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #mocking }
RBAbstractTransformationTest >> changeMockClass [

	^ RBRefactoringChangeMock
]

{ #category : #mocking }
RBAbstractTransformationTest >> objectClassVariable [

	^ Object classPool keys detect: [:each | true]
]

{ #category : #parsing }
RBAbstractTransformationTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #parsing }
RBAbstractTransformationTest >> parserClass [
	^ RBParser
]

{ #category : #running }
RBAbstractTransformationTest >> perform: aChange do: aBlock [
	"Perform a change in the system silently, evaluate aBlock and then undo the change again."

	| undo |
	^ SystemAnnouncer uniqueInstance suspendAllWhile: [
		undo := aChange execute.
		aBlock ensure: [ undo execute ] ]
]

{ #category : #running }
RBAbstractTransformationTest >> proceedThroughWarning: aBlock [

	aBlock
		on: RBRefactoringWarning
		do: [ :ex | ex resume ]
]

{ #category : #running }
RBAbstractTransformationTest >> setUp [

	super setUp.
	model := RBNamespace new
]

{ #category : #running }
RBAbstractTransformationTest >> shouldFail: aRefactoring [

	self proceedThroughWarning: [
		self should: [ aRefactoring transform ] raise: RBRefactoringError ]
]

{ #category : #tests }
RBAbstractTransformationTest >> testAPI [
	"all classes must implement #storeOn: and #transform"

	| incompleteTransformations |
	incompleteTransformations := RBTransformation allSubclasses
		select: [ :each |
			| selectors |
			selectors := each methods collect: [:m | m selector].
			each subclasses isEmpty and: [ (selectors includes: #privateTransform) not and: [ (selectors includes: #storeOn:) not ] ] ].
	self assertEmpty: incompleteTransformations.

	incompleteTransformations := RBCompositeTransformation allSubclasses
		select: [ :each |
			| selectors |
			selectors := each methods collect: [:m | m selector].
			each subclasses isEmpty and: [ (selectors includes: #buildTransformations) not ] ].
	self assertEmpty: incompleteTransformations
]
