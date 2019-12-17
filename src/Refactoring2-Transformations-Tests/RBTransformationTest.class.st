Class {
	#name : #RBTransformationTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'manager',
		'model'
	],
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #mocking }
RBTransformationTest >> abstractVariableTestData [

	| newModel classEnvironment classes |
	classes := #(#Bar #Foo) 
		inject: OrderedCollection new 
		into: [ :sum :each | 
			testingEnvironment at: each ifPresent: [ :class | 
				sum add: class; add: class class ].
			sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	newModel := RBNamespace2 onEnvironment: classEnvironment not.
	newModel name: 'Test'.
	
	#('Object subclass: #Foo
		instanceVariableNames: ''instVarName1 instVarName2''
		classVariableNames: ''ClassVarName1 ClassVarName2 ''
		poolDictionaries: ''''
		category: ''Testing'' '
	'Foo subclass: #Bar
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' '
	) do: [ :each | newModel defineClass: each ].
	
	#(#(#Bar #(
		#('instVarName1
			^1242321' #tests) 
		#('foo
			instVarName1 := instVarName1 + instVarName2 + ClassVarName1' #tests))) 
	#(#Foo #(
		#('foo
			^instVarName2 := 3' #tests)
		#('bar
			"Add one to instVarName1"
			instVarName1 := instVarName1 + 1' #tests) 
		#('classVarName1
			^ClassVarName1' #tests)
		#('instVarName1: anObject
			^anObject' #tests)
		#('asdf
			^self classVarName1: (ClassVarName1 := ClassVarName1 + 1)' #tests) 
		#('instVarName2
			^instVarName2' #tests)
		#('instVarName2: anObject
			instVarName2 := anObject' #tests)
		#('classVarName1: anObject
			^ClassVarName1 := anObject' #tests)))
	
	#('Bar class' #(
		#('classVarName2: anObject
			ClassVarName2 := anObject' #tests) 
		#('classVarName2
			^ClassVarName2' #tests))) 
	#('Foo class' #(
		#('foo
			^ClassVarName1 := ClassVarName1 * ClassVarName1 * ClassVarName1' #tests)))) 
		do: [:each | 
			| class |
			class := newModel classNamed: each first.
			each last do: [ :methodPair | 
				class 
					compile: methodPair first
					classified: methodPair last]].
	^ newModel
]

{ #category : #mocking }
RBTransformationTest >> changeMock [

	^ testingEnvironment at: #RBRefactoringChangeMock
]

{ #category : #mocking }
RBTransformationTest >> createMockClass [

	self class compiler evaluate: 'Object subclass: #RBRefactoringChangeMock
	instanceVariableNames: ''instVar''
	classVariableNames: ''ClassVar''
	poolDictionaries: ''''
	category: ''Refactoring-Tests-Changes'''.
	
	self class compiler evaluate:  'RBRefactoringChangeMock class
	instanceVariableNames: ''classInstVar'''.
	
	self changeMock compile: 'one ^ 1' classified: 'accessing'.
]

{ #category : #mocking }
RBTransformationTest >> objectClassVariable [

	^ Object classPool keys detect: [:each | true]
]

{ #category : #parsing }
RBTransformationTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #parsing }
RBTransformationTest >> parserClass [
	^ RBParser
]

{ #category : #running }
RBTransformationTest >> perform: aChange do: aBlock [
	"Perform a change in the system silently, evaluate aBlock and then undo the change again."

	| undo |
	^ SystemAnnouncer uniqueInstance suspendAllWhile: [
		undo := aChange execute.
		aBlock ensure: [ undo execute ] ]
]

{ #category : #running }
RBTransformationTest >> proceedThroughWarning: aBlock [

	aBlock
		on: RBRefactoringError
		do: [ :ex | ex resume ]
]

{ #category : #mocking }
RBTransformationTest >> removeMockClass [

	testingEnvironment removeClassNamed: #RBRefactoringChangeMock.
]

{ #category : #running }
RBTransformationTest >> setUp [

	super setUp.
	self createMockClass.
	model := RBNamespace2 new
]

{ #category : #running }
RBTransformationTest >> shouldFail: aRefactoring [ 

	self proceedThroughWarning: [
		self should: [ aRefactoring transform ] raise: RBRefactoringError ]
]

{ #category : #running }
RBTransformationTest >> tearDown [

	self removeMockClass.
	super tearDown
]

{ #category : #testing }
RBTransformationTest >> testAPI [
	"all classes must implement #storeOn: and #transform"

	| incompleteTransformations |
	incompleteTransformations := RBTransformation allSubclasses
		select: [ :each | 
			| selectors |
			selectors := each methods collect: #selector.

			each subclasses isEmpty and: [ (selectors includes: #privateTransform) not and: [ (selectors includes: #storeOn:) not ] ] ].
	self assertEmpty: incompleteTransformations.

	incompleteTransformations := RBCompositeTransformation allSubclasses
		select: [ :each | 
			| selectors |
			selectors := each methods collect: #selector.

			each subclasses isEmpty and: [ (selectors includes: #buildTransformations) not ] ].
	self assertEmpty: incompleteTransformations
]
