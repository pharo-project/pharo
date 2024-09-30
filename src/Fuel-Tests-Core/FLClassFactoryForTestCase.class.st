"
I am a custom class factory used by the Fuel test suite. I was originally a copy of ClassFactoryForTestCase.

I'm useful when classes needs to be created during the execution of the test. This avoids polluting your unit tests with dummy and mock classes.
A typical usage of it is:

TestCase subclass: #YourTest
instanceVariableNames: 'classFactory'

YourTest>>setUp
classFactory := FLClassFactoryForTestCase new

YourTest>>tearDown
classFactory cleanUp.

YourTest>>testIsBehavior
| cls |
cls := classFactory newClass.
self assert: cls isBehavior
"
Class {
	#name : #FLClassFactoryForTestCase,
	#superclass : #Object,
	#instVars : [
		#createdClasses,
		#createdTraits,
		#environment
	],
	#classVars : [
		#Counter
	],
	#category : #'Fuel-Tests-Core-Support-ClassFactory'
}

{ #category : #'instance creation' }
FLClassFactoryForTestCase class >> forEnvironment: anEnvironment [
	^ self basicNew
		initializeWithEnvironment: anEnvironment;
		initialize;
		yourself
]

{ #category : #cleaning }
FLClassFactoryForTestCase >> cleanUp [
	| createdClassNames createdTraitNames |
	createdClassNames := self createdClassNames.
	createdTraitNames := self createdTraitNames.
	self deleteClasses.
	self deleteTraits.
	self cleanUpSystemOrganization.
	self deletePackage.
	self cleanUpChangeSetForClassNames: createdClassNames.
	self cleanUpChangeSetForClassNames: createdTraitNames
]

{ #category : #'private-cleaning' }
FLClassFactoryForTestCase >> cleanUpChangeSetForClassNames: classeNames [

	self class environment at: #ChangeSet ifPresent: [ :aChangeSetClass |
		| changeSet |
		changeSet := aChangeSetClass current.
		classeNames do: [:name|	
			changeSet 
				removeClassChanges: name;
				removeClassChanges: name, ' class']]
]

{ #category : #'private-cleaning' }
FLClassFactoryForTestCase >> cleanUpSystemOrganization [
	"This method is necessary because we create classes in a non-default environment. The
	SystemOrganization is global and ClassFactoryForTestCase interacts with it to clean out
	categories."
	| categoriesMatchString categories |
	categoriesMatchString := self defaultPackageName, '-*'.
	categories := SystemOrganization categoriesMatching: categoriesMatchString.
	categories do: [ :category |
		(SystemOrganization listAtCategoryNamed: category) do: [ :behavior |
			SystemOrganization removeElement: behavior ] ]
]

{ #category : #accessing }
FLClassFactoryForTestCase >> createdClassNames [
	^self createdClasses collect: [ :class| class name ]
]

{ #category : #accessing }
FLClassFactoryForTestCase >> createdClasses [
	^createdClasses
]

{ #category : #accessing }
FLClassFactoryForTestCase >> createdTraitNames [
	^self createdTraits collect: [ :trait | trait name ]
]

{ #category : #accessing }
FLClassFactoryForTestCase >> createdTraits [
	^createdTraits
]

{ #category : #'accessing-defaults' }
FLClassFactoryForTestCase >> defaultNewClassName [

	| postFix |
	postFix := self nextCount printString.
	^ (#ClassForTestToBeDeleted, postFix) asSymbol.
]

{ #category : #'accessing-defaults' }
FLClassFactoryForTestCase >> defaultNewTraitName [

	| postFix |
	postFix := (self createdTraits size + 1) printString.
	^ (#TraitForTestToBeDeleted, postFix) asSymbol
]

{ #category : #'accessing-defaults' }
FLClassFactoryForTestCase >> defaultPackageAndTag [
	^ (self defaultPackageName , '-', self defaultTagPostfix) asSymbol
]

{ #category : #'accessing-defaults' }
FLClassFactoryForTestCase >> defaultPackageName [

	^ #CategoryForTestToBeDeleted
]

{ #category : #'accessing-defaults' }
FLClassFactoryForTestCase >> defaultSuperclass [ 
	^Object
]

{ #category : #'accessing-defaults' }
FLClassFactoryForTestCase >> defaultTagPostfix [
	^ #Default
]

{ #category : #cleaning }
FLClassFactoryForTestCase >> deleteBehavior: aBehavior [
	self createdClasses
		remove: aBehavior
		ifAbsent: [].
	self createdTraits
		remove: aBehavior
		ifAbsent: [].
	self removeFromSystem: aBehavior
]

{ #category : #'private-cleaning' }
FLClassFactoryForTestCase >> deleteClasses [
	self createdClasses do: [:class|
		self deleteBehavior: class]
]

{ #category : #'private-cleaning' }
FLClassFactoryForTestCase >> deletePackage [
	| categoriesMatchString |
	categoriesMatchString := self defaultPackageName, '-*'.
	SystemOrganization removeCategoriesMatching: categoriesMatchString
]

{ #category : #'private-cleaning' }
FLClassFactoryForTestCase >> deleteTraits [
	self createdTraits do: [:trait|
		self deleteBehavior: trait]
]

{ #category : #accessing }
FLClassFactoryForTestCase >> environment [
	^ environment
]

{ #category : #initialization }
FLClassFactoryForTestCase >> initialize [
	super initialize.
	createdClasses := IdentitySet new.
	createdTraits := IdentitySet new.
	environment ifNil: [
		environment := self class environment ]
]

{ #category : #initialization }
FLClassFactoryForTestCase >> initializeWithEnvironment: anEnvironment [
	environment := anEnvironment
]

{ #category : #'creating-anonymous' }
FLClassFactoryForTestCase >> newAnonymousClass [
	^ FLPlatform current
		newAnonymousSubclassOf: self defaultSuperclass
		named: 'ignored'
]

{ #category : #'creating-traits-anonymous' }
FLClassFactoryForTestCase >> newAnonymousTrait [
	^ FLPlatform current newAnonymousTraitNamed: self defaultNewTraitName
]

{ #category : #creating }
FLClassFactoryForTestCase >> newClass [
	^ self newSubclassOf: self defaultSuperclass
]

{ #category : #creating }
FLClassFactoryForTestCase >> newClassWithClassVariableNames: aString [

	^ self 
		newSubclassOf: self defaultSuperclass
		instanceVariableNames: ''
		classVariableNames: aString
]

{ #category : #creating }
FLClassFactoryForTestCase >> newClassWithInstanceVariableNames: aString [

	^ self 
		newSubclassOf: self defaultSuperclass
		instanceVariableNames: aString
		classVariableNames: ''
]

{ #category : #creating }
FLClassFactoryForTestCase >> newClassWithInstanceVariableNames: aString classVariableNames: anotherString [

	^ self 
		newSubclassOf: self defaultSuperclass
		instanceVariableNames: aString
		classVariableNames: anotherString
]

{ #category : #creating }
FLClassFactoryForTestCase >> newSubclassOf: aClass [

	^ self 
		newSubclassOf: aClass
		instanceVariableNames: ''
		classVariableNames: ''
]

{ #category : #creating }
FLClassFactoryForTestCase >> newSubclassOf: aClass instanceVariableNames: ivNamesString [
	^ self 
		newSubclassOf: aClass
		instanceVariableNames: ivNamesString 
		classVariableNames: ''
]

{ #category : #creating }
FLClassFactoryForTestCase >> newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString [
	^ self 
		newSubclassOf: aClass 
		uses: { }
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString
]

{ #category : #creating }
FLClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition [
	^ self 
		newSubclassOf: aClass
		uses: aTraitComposition
		instanceVariableNames: ''
		classVariableNames: ''
]

{ #category : #creating }
FLClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString [

	^ self
		  newSubclassOf: aClass
		  uses: aTraitComposition
		  instanceVariableNames: ivNamesString
		  classVariableNames: classVarsString
		  poolDictionaries: ''
]

{ #category : #creating }
FLClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: poolNamesString [
	| newClass |		
	newClass := FLPlatform current
		newSubclassOf: aClass
		named: self defaultNewClassName
		uses: aTraitComposition
		instanceVariableNames: ivNamesString
		classVariableNames: classVarsString
		poolDictionaries: poolNamesString
		environment: self environment
		package: self defaultPackageAndTag.
		
	self createdClasses add: newClass.
	^ newClass
]

{ #category : #'creating-traits' }
FLClassFactoryForTestCase >> newTrait [
	^ self
		newTraitNamed: self defaultNewTraitName
		uses: Array new
		instanceVariableNames: ''
]

{ #category : #'creating-traits' }
FLClassFactoryForTestCase >> newTraitNamed: aTraitName uses: aTraitComposition instanceVariableNames: aString [
	| newTrait |					
	newTrait := FLPlatform current
		newTraitNamed: aTraitName
		uses: aTraitComposition
		instanceVariableNames: aString
		environment: self environment
		package: self defaultPackageAndTag.
	
	self createdTraits add: newTrait.
	^ newTrait
]

{ #category : #accessing }
FLClassFactoryForTestCase >> nextCount [
	"Global counter to avoid name clash between test runs, in case of some previous failure."
	^ Counter := (Counter ifNil: [ 0 ]) + 1.
]

{ #category : #accessing }
FLClassFactoryForTestCase >> registerBehavior: aBehavior [
	(aBehavior isTrait
		ifTrue: [ self createdTraits ]
		ifFalse: [ self createdClasses ]) add: aBehavior
]

{ #category : #'private-cleaning' }
FLClassFactoryForTestCase >> removeFromSystem: aBehavior [
	aBehavior isObsolete	ifTrue: [ ^ self ].
	
	self class environment
		at: #ChangeSet
		ifPresent: [ :changeSet |
			aBehavior removeFromChanges ].
	
	aBehavior removeFromSystemUnlogged
]

{ #category : #'compiling-silently' }
FLClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior [
	^ self
		silentlyCompile: aString
		in: aBehavior
		storingSource: true
]

{ #category : #'compiling-silently' }
FLClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior category: anotherString [
	^ self
		silentlyCompile: aString
		in: aBehavior
		category: anotherString
		storingSource: true
]

{ #category : #'compiling-silently' }
FLClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior category: anotherString storingSource: aBoolean [
	^ self silentlyDo: [
		FLPlatform current 
			useAuthor: 'TestsAuthor'
			during: [
				aBehavior
					compile: aString
					classified: anotherString
					withStamp: nil 
					notifying: nil 
					logSource: aBoolean ] ]
]

{ #category : #'compiling-silently' }
FLClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior storingSource: aBoolean [
	^ self
		silentlyCompile: aString
		in: aBehavior
		category: 'unclassified'
		storingSource: aBoolean
]

{ #category : #private }
FLClassFactoryForTestCase >> silentlyDo: aBlock [
	^ FLPlatform current silentlyDo: aBlock
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewClass [
	^ self silentlyNewSubclassOf: self defaultSuperclass
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewClassWithClassVariableNames: aString [
	^ self
		silentlyNewSubclassOf: self defaultSuperclass
		instanceVariableNames: ''
		classVariableNames: aString
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewClassWithInstanceVariableNames: aString [
	^ self
		silentlyNewSubclassOf: self defaultSuperclass
		instanceVariableNames: aString
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewClassWithInstanceVariableNames: aString classVariableNames: anotherString [
	^ self
		silentlyNewSubclassOf: self defaultSuperclass
		instanceVariableNames: aString
		classVariableNames: anotherString
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass [
	^ self
		silentlyNewSubclassOf: aClass
		instanceVariableNames: ''
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: aString [
	^ self
		silentlyNewSubclassOf: aClass
		instanceVariableNames: aString
		classVariableNames: ''
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString [
	^ self
		silentlyNewSubclassOf: aClass
		uses: #()
		instanceVariableNames: ivNamesString
		classVariableNames: classVarsString
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass uses: aTraitComposition [
	^ self
		  silentlyNewSubclassOf: aClass
		  uses: aTraitComposition
		  instanceVariableNames: ''
		  classVariableNames: ''
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString [
	^ self
		silentlyNewSubclassOf: aClass
		uses: aTraitComposition
		instanceVariableNames: ivNamesString
		classVariableNames: classVarsString
		poolDictionaries: ''
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: poolNamesString [
	^ self silentlyDo: [
		self
			newSubclassOf: aClass
			uses: aTraitComposition
			instanceVariableNames: ivNamesString
			classVariableNames: classVarsString
			poolDictionaries: poolNamesString ]
]

{ #category : #'creating-traits-silently' }
FLClassFactoryForTestCase >> silentlyNewTrait [
	^ self
		silentlyNewTraitNamed: self defaultNewTraitName
		uses: Array new
		instanceVariableNames: ''
]

{ #category : #'creating-traits-silently' }
FLClassFactoryForTestCase >> silentlyNewTraitNamed: aTraitName uses: aTraitComposition instanceVariableNames: aString [
	^ self silentlyDo: [			
		self
			newTraitNamed: aTraitName
			uses: aTraitComposition
			instanceVariableNames: aString ]
]

{ #category : #'creating-traits-silently' }
FLClassFactoryForTestCase >> silentlyNewTraitWithInstanceVariableNames: aString [
	^ self
		silentlyNewTraitNamed: self defaultNewTraitName
		uses: Array new
		instanceVariableNames: aString
]

{ #category : #'renaming-silently' }
FLClassFactoryForTestCase >> silentlyRename: aClass to: aName [
	^ self silentlyDo: [ aClass rename: aName asSymbol ]
]
