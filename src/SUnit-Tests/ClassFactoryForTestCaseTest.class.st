"
SUnit tests for ClassFactoryForTestCase 
"
Class {
	#name : #ClassFactoryForTestCaseTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'factory'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #accessing }
ClassFactoryForTestCaseTest class >> defaultTimeLimit [
	^10 seconds
]

{ #category : #history }
ClassFactoryForTestCaseTest class >> lastStoredRun [
	^ ((Dictionary new) add: (#passed->((Set new) add: #testDefaultCategoryCleanUp; add: #testPackageCleanUp; add: #testSingleClassCreation; add: #testClassCreationInDifferentCategories; add: #testClassFastCreationInDifferentCategories; add: #testMultipleClassCreation; add: #testSingleClassFastCreation; yourself)); add: (#timeStamp->'22 November 2008 10:11:35 pm'); add: (#failures->((Set new))); add: (#errors->((Set new))); yourself)
]

{ #category : #running }
ClassFactoryForTestCaseTest >> setUp [
	super setUp.
	factory := ClassFactoryForTestCase new
]

{ #category : #running }
ClassFactoryForTestCaseTest >> tearDown [
	
	factory cleanUp.
	super tearDown
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testClassCreationInDifferentCategories [
	| firstThreeClasses lastTwoClasses |
	3 timesRepeat: [
		factory newSubclassOf: Object instanceVariableNames: '' classVariableNames: '' category: #One].
	firstThreeClasses := factory createdClasses copy.
	2 timesRepeat: [
		factory newSubclassOf: Object instanceVariableNames: '' classVariableNames: '' category: #Two].
	lastTwoClasses := factory createdClasses copyWithoutAll: firstThreeClasses.
	self assert: (firstThreeClasses allSatisfy: [:class| class category = (factory packageName, '-', #One) asSymbol]).
	self assert: (lastTwoClasses allSatisfy: [:class| class category = (factory packageName, '-', #Two) asSymbol]).
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testClassFastCreationInDifferentCategories [
	| firstThreeClasses lastTwoClasses |
	3 timesRepeat: [
		factory newClassInCategory: #One].
	firstThreeClasses := factory createdClasses copy.
	2 timesRepeat: [
		factory newClassInCategory: #Two].
	lastTwoClasses := factory createdClasses copyWithoutAll: firstThreeClasses.
	self assert: (firstThreeClasses allSatisfy: [:class| class category = (factory packageName, '-', #One) asSymbol]).
	self assert: (lastTwoClasses allSatisfy: [:class| class category = (factory packageName, '-', #Two) asSymbol]).
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testDefaultCategoryCleanUp [
	| createdClassNames allClasses allTraits |
	
	3 timesRepeat: [
		factory newClass.
		factory newTrait].
	
	createdClassNames := factory createdClassNames.
	
	factory cleanUp.	
	
	self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]). 
	self assert: (factory createdTraits allSatisfy: [:trait| trait isObsolete]). 
	
	allClasses := SystemNavigation new allClasses.
	allTraits := testingEnvironment allTraits.
	
	self assert: (factory createdClasses noneSatisfy: [:class| allClasses includes: class]).
	self assert: (factory createdTraits noneSatisfy: [:trait| allTraits includes: trait]).
	self deny: (SystemOrganization categories includes: factory defaultCategory). 
	self class environment at: #ChangeSet ifPresent: [:changeSet |
		self deny: (changeSet current changedClassNames includesAnyOf: createdClassNames) ].

]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testDuplicateClassWithNewName [
	| createdClass |
	
	"Skip this tests if no compiler is available. Do not use #skip because it breaks an announcements tests"
	
	Smalltalk compilerClass ifNotNil: [ 	
		createdClass := factory duplicateClass: TestCase withNewName: #MyTestClass.
		self assert: (factory createdClasses allSatisfy: [:class| self class environment includesKey: class name ]).
		factory cleanUp.
		self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]).
		self deny: (ChangeSet current changedClassNames includes: createdClass).]
	

]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testMultipleClassCreation [
	5 timesRepeat: [ factory newClass ].
	self assert: (SystemNavigation new allClasses includesAll: factory createdClasses).
	self assert: factory createdClassNames asSet size equals: 5.
	self assert: (SystemOrganization listAtCategoryNamed: factory defaultCategory) asSet equals: factory createdClassNames asSet
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testPackageCleanUp [
	| createdClassNames allClasses |
	3 timesRepeat: [ factory newClassInCategory: #One ].
	2 timesRepeat: [ factory newClassInCategory: #Two ].
	createdClassNames := factory createdClassNames.
	factory cleanUp.
	self assert: (factory createdClasses allSatisfy: [ :class | class isObsolete ]).
	allClasses := SystemNavigation new allClasses.
	self assert: (factory createdClasses noneSatisfy: [ :class | allClasses includes: class ]).
	self assertEmpty: (SystemOrganization categoriesMatching: factory packageName , '*').
	self class environment at: #ChangeSet ifPresent: [ :changeSet | self deny: (changeSet current changedClassNames includesAnyOf: createdClassNames) ]
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testSingleClassCreation [
	| class elementsInCategoryForTest |
	class := factory newSubclassOf: Object instanceVariableNames: 'a b c' classVariableNames: 'X Y'.
	self assert: (SystemNavigation new allClasses includes: class).
	elementsInCategoryForTest := SystemOrganization listAtCategoryNamed: factory defaultCategory.
	self assert: elementsInCategoryForTest equals: {class name}.
	self assert: class instVarNames equals: #(a b c).
	self assert: class classPool keys asSet equals: #(X Y) asSet
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testSingleClassFastCreation [
	| class elementsInCategoryForTest |
	class := factory newClass.
	self assert: (SystemNavigation new allClasses includes: class).
	elementsInCategoryForTest := SystemOrganization listAtCategoryNamed: factory defaultCategory.
	self assert: elementsInCategoryForTest equals: {class name}.
	self assertEmpty: class instVarNames.
	self assertEmpty: class classPool
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testSingleTraitCreation [
	| trait |
	trait := factory newTrait.
	self assert: (testingEnvironment allTraits includes: trait).
	self assert: trait traitUsers size equals: 0.
	self assert: trait traits size equals: 0
]

{ #category : #testing }
ClassFactoryForTestCaseTest >> testTraitCreationInDifferentCategories [
	| firstThreeTraits lastTwoTraits |
	3 timesRepeat: [
		factory newTraitInTag: #One].
	firstThreeTraits := factory createdTraits copy.
	2 timesRepeat: [
		factory newTraitInTag: #Two].
	lastTwoTraits := factory createdTraits copyWithoutAll: firstThreeTraits.
	self assert: (firstThreeTraits allSatisfy: [:trait| trait category = (factory packageName, '-', #One) asSymbol]).
	self assert: (lastTwoTraits allSatisfy: [:trait| trait category = (factory packageName, '-', #Two) asSymbol]).
]
