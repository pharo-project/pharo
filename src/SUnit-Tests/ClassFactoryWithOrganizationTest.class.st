"
SUnit tests for ClassFactoryWithOrganization
"
Class {
	#name : #ClassFactoryWithOrganizationTest,
	#superclass : #TestCase,
	#instVars : [
		'factory'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #accessing }
ClassFactoryWithOrganizationTest class >> defaultTimeLimit [ 
	^10 seconds
]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> assertEnvironmentOf: aBehavior [
	self assert: aBehavior environment equals: self testedEnvironment
]

{ #category : #running }
ClassFactoryWithOrganizationTest >> setUp [
	| environment |
	super setUp.
	environment := SystemDictionary withOrganizer: SystemOrganizer new.
	factory := ClassFactoryWithOrganization newWithOrganization: environment organization.
	
]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> testClassCreationInDifferentCategories [
	| firstThreeClasses lastTwoClasses |
	3 timesRepeat: [
		factory newSubclassOf: Object instanceVariableNames: '' classVariableNames: '' category: #One].
	firstThreeClasses := factory createdClasses copy.
	2 timesRepeat: [
		factory newSubclassOf: Object instanceVariableNames: '' classVariableNames: '' category: #Two].
	lastTwoClasses := factory createdClasses copyWithoutAll: firstThreeClasses.
	self assert: (firstThreeClasses allSatisfy: [:class| class category = #One]).
	self assert: (lastTwoClasses allSatisfy: [:class| class category = #Two]).
	factory createdClasses do: [ :aClass |
		self assertEnvironmentOf: aClass ]
]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> testClassFastCreationInDifferentCategories [
	| firstThreeClasses lastTwoClasses |
	3 timesRepeat: [
		factory newClassInCategory: #One].
	firstThreeClasses := factory createdClasses copy.
	2 timesRepeat: [
		factory newClassInCategory: #Two].
	lastTwoClasses := factory createdClasses copyWithoutAll: firstThreeClasses.
	self assert: (firstThreeClasses allSatisfy: [:class| class category =  #One]).
	self assert: (lastTwoClasses allSatisfy: [:class| class category = #Two]).
	factory createdClasses do: [ :aClass |
		self assertEnvironmentOf: aClass ]
]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> testDefaultCategoryCleanUp [
	| createdClassNames allClasses |
	3 timesRepeat: [
		factory newClass].
	createdClassNames := factory createdClassNames.
	factory cleanUp.	
	self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]). 
	allClasses := self testedEnvironment allClasses.
	self assert: (factory createdClasses noneSatisfy: [:class| allClasses includes: class]).
	self deny: (self testedOrganization categories includes: factory defaultCategory). 
	self class environment at: #ChangeSet ifPresent: [:changeSet |
		self deny: (changeSet current changedClassNames includesAnyOf: createdClassNames) ].

]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> testMultipleClassCreation [
	5 timesRepeat: [ factory newClass ].
	self assert: (self testedEnvironment allClasses includesAll: factory createdClasses).
	self assert: factory createdClassNames asSet size equals: 5.
	self assert: (self testedOrganization listAtCategoryNamed: factory defaultCategory) asSet equals: factory createdClassNames asSet.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ]
]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> testPackageCleanUp [
	| createdClassNames allClasses |
	3 timesRepeat: [ factory newClassInCategory: #One ].
	2 timesRepeat: [ factory newClassInCategory: #Two ].
	createdClassNames := factory createdClassNames.
	factory cleanUp.
	self assert: (factory createdClasses allSatisfy: [ :class | class isObsolete ]).
	allClasses := self testedEnvironment allClasses.
	self assert: (factory createdClasses noneSatisfy: [ :class | allClasses includes: class ]).
	self assertEmpty: (self testedOrganization categoriesMatching: factory packageName , '*').
	self class environment at: #ChangeSet ifPresent: [ :changeSet | self deny: (changeSet current changedClassNames includesAnyOf: createdClassNames) ]
]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> testSingleClassCreation [
	| class elementsInCategoryForTest |
	class := factory newSubclassOf: Object instanceVariableNames: 'a b c' classVariableNames: 'X Y'.
	self assert: (self testedEnvironment allClasses includes: class).
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ].
	elementsInCategoryForTest := self testedOrganization listAtCategoryNamed: factory defaultCategory.
	self assert: elementsInCategoryForTest equals: {class name}.
	self assert: class instVarNames equals: #(a b c).
	self assert: class classPool keys asSet equals: #(X Y) asSet
]

{ #category : #testing }
ClassFactoryWithOrganizationTest >> testSingleClassFastCreation [
	| class elementsInCategoryForTest |
	class := factory newClass.
	self assert: (self testedEnvironment allClasses includes: class).
	elementsInCategoryForTest := self testedOrganization listAtCategoryNamed: factory defaultCategory.
	factory createdClasses do: [ :aClass | self assertEnvironmentOf: aClass ].
	self assert: elementsInCategoryForTest equals: {class name}.
	self assertEmpty: class instVarNames.
	self assertEmpty: class classPool
]

{ #category : #accessing }
ClassFactoryWithOrganizationTest >> testedEnvironment [
	^self testedOrganization environment
]

{ #category : #accessing }
ClassFactoryWithOrganizationTest >> testedOrganization [
	^factory organization
]
