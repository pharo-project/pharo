Class {
	#name : #RBAbstractTransformationTest,
	#superclass : #RBAbstractEnvTestCase,
	#traits : 'TModelBuilder',
	#classTraits : 'TModelBuilder classTrait',
	#instVars : [
		'model'
	],
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #mocking }
RBAbstractTransformationTest >> changeMock [

	^ testingEnvironment at: #RBRefactoringChangeMock
]

{ #category : #mocking }
RBAbstractTransformationTest >> createMockClass [

	self class classInstaller make: [ :aClassBuilder |
		aClassBuilder
			name: 'RBRefactoringChangeMock';
			sharedVariablesFromString: 'ClassVar';
			slots: 'instVar' asSlotCollection;
			classSlots: 'classInstVar' asSlotCollection;
			package: 'Refactoring-Tests-Changes' ].

	self changeMock compile: 'one ^ 1' classified: 'accessing'
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

{ #category : #mocking }
RBAbstractTransformationTest >> removeMockClass [

	testingEnvironment removeClassNamed: #RBRefactoringChangeMock
]

{ #category : #running }
RBAbstractTransformationTest >> setUp [

	super setUp.
	self createMockClass.
	model := RBNamespace new
]

{ #category : #running }
RBAbstractTransformationTest >> shouldFail: aRefactoring [

	self proceedThroughWarning: [
		self should: [ aRefactoring transform ] raise: RBRefactoringError ]
]

{ #category : #running }
RBAbstractTransformationTest >> tearDown [

	self removeMockClass.
	super tearDown
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
