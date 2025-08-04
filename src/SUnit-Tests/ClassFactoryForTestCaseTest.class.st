"
SUnit tests for ClassFactoryForTestCase 
"
Class {
	#name : 'ClassFactoryForTestCaseTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'factory'
	],
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
ClassFactoryForTestCaseTest class >> shouldInheritSelectors [

	^ true
]

{ #category : 'running' }
ClassFactoryForTestCaseTest >> environment [

	^ testingEnvironment
]

{ #category : 'running' }
ClassFactoryForTestCaseTest >> organization [

	^ self environment organization
]

{ #category : 'running' }
ClassFactoryForTestCaseTest >> setUp [
	super setUp.
	factory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ClassFactoryForTestCaseTest >> tearDown [

	factory cleanUp.
	super tearDown
]

{ #category : 'testing' }
ClassFactoryForTestCaseTest >> testClassCreationInDifferentTags [

	| firstThreeClasses lastTwoClasses |
	3 timesRepeat: [ factory make: [ :aBuilder | aBuilder tag: #One ] ].
	firstThreeClasses := factory createdClasses copy.
	2 timesRepeat: [ factory make: [ :aBuilder | aBuilder tag: #Two ] ].
	lastTwoClasses := factory createdClasses copyWithoutAll: firstThreeClasses.
	self assert: (firstThreeClasses allSatisfy: [ :class | class package name = factory packageName and: [ class packageTag name = #One ] ]).
	self assert: (lastTwoClasses allSatisfy: [ :class | class package name = factory packageName and: [ class packageTag name = #Two ] ])
]

{ #category : 'testing' }
ClassFactoryForTestCaseTest >> testMultipleClassCreation [

	5 timesRepeat: [ factory newClass ].
	self assert: (self environment allClasses includesAll: factory createdClasses).
	self assert: factory createdClassNames asSet size equals: 5.
	self assertCollection: (self organization packageNamed: factory packageName) definedClasses hasSameElements: factory createdClasses
]

{ #category : 'testing' }
ClassFactoryForTestCaseTest >> testPackageCleanUp [

	| createdClassNames allClasses |
	3 timesRepeat: [ factory make: [ :aBuilder | aBuilder tag: #One ] ].
	2 timesRepeat: [ factory make: [ :aBuilder | aBuilder tag: #Two ] ].
	createdClassNames := factory createdClassNames.
	factory cleanUp.
	self assert: (factory createdClasses allSatisfy: [ :class | class isObsolete ]).
	allClasses := self environment allClasses.
	self assert: (factory createdClasses noneSatisfy: [ :class | allClasses includes: class ]).
	self assertEmpty: (self organization packages select: [ :package | package name beginsWith: factory packageName ])
]

{ #category : 'testing' }
ClassFactoryForTestCaseTest >> testPackageOfCreatedClassIsCleanedUp [

	| class |
	class := factory make: [ :aBuilder |
		         aBuilder
			         superclass: Object;
			         package: 'ClassFactoryForTestCaseGeneratedPackage' ].
	self assert: class package name equals: 'ClassFactoryForTestCaseGeneratedPackage'.
	self assert: (self organization packages anySatisfy: [ :package | package name = 'ClassFactoryForTestCaseGeneratedPackage' ]).
	factory cleanUp.
	self deny: (self organization packages anySatisfy: [ :package | package name = 'ClassFactoryForTestCaseGeneratedPackage' ])
]

{ #category : 'testing' }
ClassFactoryForTestCaseTest >> testSingleClassCreation [

	| class |
	class := factory make: [ :aBuilder |
		         aBuilder
			         superclass: Object;
			         slots: #(a b c);
			         sharedVariables: #(X Y);
			         tag: factory tagName ].
	self assert: (self environment allClasses includes: class).
	self assert: class package name equals: factory packageName.
	self assert: class packageTag name equals: factory tagName.
	self assert: class instVarNames equals: #( a b c ).
	self assert: class classVarNames asSet equals: #( X Y ) asSet
]

{ #category : 'testing' }
ClassFactoryForTestCaseTest >> testSingleTraitCreation [

	| trait |
	trait := factory newTrait.
	self assert: (self environment allTraits includes: trait).
	self assertEmpty: trait traitUsers.
	self assertEmpty: trait traits
]

{ #category : 'testing' }
ClassFactoryForTestCaseTest >> testTraitCreationInDifferentCategories [

	| firstThreeTraits lastTwoTraits |
	3 timesRepeat: [
		factory make: [ :aBuilder |
			aBuilder
				beTrait;
				tag: #One ] ].
	firstThreeTraits := factory createdTraits copy.
	2 timesRepeat: [
		factory make: [ :aBuilder |
			aBuilder
				beTrait;
				tag: #Two ] ].
	lastTwoTraits := factory createdTraits copyWithoutAll: firstThreeTraits.
	self assert: (firstThreeTraits allSatisfy: [ :trait | trait package name = factory packageName and: [ trait packageTag name = #One ] ]).
	self assert: (lastTwoTraits allSatisfy: [ :trait | trait package name = factory packageName and: [ trait packageTag name = #Two ] ])
]
