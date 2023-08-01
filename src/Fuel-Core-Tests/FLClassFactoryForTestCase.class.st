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
	#superclass : #ClassFactoryForTestCase,
	#instVars : [
		'createdClasses',
		'createdTraits'
	],
	#category : #'Fuel-Core-Tests-Support-ClassFactory'
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
FLClassFactoryForTestCase >> defaultPackageAndTag [
	^ (self packageName , '-', self defaultTagPostfix) asSymbol
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

	self packageOrganizer removePackage: self packageName
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
	^ self defaultSuperclass newAnonymousSubclass
]

{ #category : #'creating-traits-anonymous' }
FLClassFactoryForTestCase >> newAnonymousTrait [
	^ Smalltalk anonymousClassInstaller make: [ :builder |
		builder
			"The name is necessary to not break Pharo"
			name: self newBehaviorName;
			beTrait ]
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
	newClass := self class classInstaller make: [ :aBuilder |
		aBuilder
			environment: self environment;
			name: self newBehaviorName;
			superclass: aClass;
			traitComposition: aTraitComposition asTraitComposition;
			classTraitComposition: aTraitComposition asTraitComposition classComposition;
			slots: ivNamesString asSlotCollection;
			sharedVariablesFromString: classVarsString;
			sharedPools: poolNamesString;
			package: self defaultPackageAndTag ].

	self createdClasses add: newClass.
	^ newClass
]

{ #category : #'creating-traits' }
FLClassFactoryForTestCase >> newTrait [
	^ self
		newTraitNamed: self newBehaviorName
		uses: Array new
		instanceVariableNames: ''
]

{ #category : #'creating-traits' }
FLClassFactoryForTestCase >> newTraitNamed: aTraitName uses: aTraitComposition instanceVariableNames: aString [
	| newTrait |
	newTrait := self class classInstaller make: [ :aBuilder |
		aBuilder
			environment: self environment;
			name: aTraitName;
			slots: aString asSlotCollection;
			traitComposition: aTraitComposition asTraitComposition;
			package: self defaultPackageAndTag;
			beTrait ].

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
		  Author
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
	^ SystemAnnouncer uniqueInstance suspendAllWhile: aBlock
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
		silentlyNewTraitNamed: self newBehaviorName
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
		silentlyNewTraitNamed: self newBehaviorName
		uses: Array new
		instanceVariableNames: aString
]

{ #category : #'renaming-silently' }
FLClassFactoryForTestCase >> silentlyRename: aClass to: aName [
	^ self silentlyDo: [ aClass rename: aName asSymbol ]
]
