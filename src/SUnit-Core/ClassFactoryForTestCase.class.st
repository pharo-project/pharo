"
I'm useful when classes needs to be created during the execution of the test. This avoid polluting your unit tests with dummy and mock classes.
A typical usage of it is:

TestCase subclass: #YourTest
       instanceVariableNames: 'classFactory'

YourTest>>setUp
       classFactory := ClassFactoryForTestCase new

YourTest>>tearDown
       classFactory deleteClasses.

YourTest>>testIsBehavior
       | cls |
       cls := classFactory newClass.
       self assert: cls isBehavior

"
Class {
	#name : #ClassFactoryForTestCase,
	#superclass : #Object,
	#instVars : [
		'createdClasses',
		'createdTraits',
		'createdSilently',
		'environment'
	],
	#classVars : [
		'Counter'
	],
	#category : #'SUnit-Core-Extensions'
}

{ #category : #'instance creation' }
ClassFactoryForTestCase class >> environment: aSystemDictionary [

	^ self new
		  environment: aSystemDictionary;
		  yourself
]

{ #category : #'class initialization' }
ClassFactoryForTestCase class >> initialize [
	"Now we can personnalize the environment of the ClassFactoryForTestCase. So instead of having this duplicated class, we can just use this one and specify the environment that will have its own organization."

	self deprecatedAliases: { #ClassFactoryWithOrganization }
]

{ #category : #cleaning }
ClassFactoryForTestCase >> cleanUp [

	| createdClassNames createdTraitNames |
	createdClassNames := self createdClassNames.
	createdTraitNames := self createdTraitNames.

	self
		deleteClasses;
		deleteTraits;
		deletePackage;
		cleanUpChangeSetForClassNames: createdClassNames;
		cleanUpChangeSetForClassNames: createdTraitNames
]

{ #category : #cleaning }
ClassFactoryForTestCase >> cleanUpChangeSetForClassNames: classeNames [

	self class environment at: #ChangeSet ifPresent: [ :aChangeSetClass |
		| changeSet |
		changeSet := aChangeSetClass current.
		classeNames do: [:name|
			changeSet
				removeClassChanges: name;
				removeClassChanges: name, ' class']]
]

{ #category : #accessing }
ClassFactoryForTestCase >> createdClassNames [

	^ self createdClasses collect: [ :class | class name ]
]

{ #category : #accessing }
ClassFactoryForTestCase >> createdClasses [

	^ createdClasses
]

{ #category : #accessing }
ClassFactoryForTestCase >> createdTraitNames [

	^ self createdTraits collect: [ :trait | trait name ]
]

{ #category : #accessing }
ClassFactoryForTestCase >> createdTraits [

	^ createdTraits
]

{ #category : #accessing }
ClassFactoryForTestCase >> defaultCategory [

	^ (self packageName , '-' , self defaultTagPostfix) asSymbol
]

{ #category : #accessing }
ClassFactoryForTestCase >> defaultSuperclass [

	^ Object
]

{ #category : #accessing }
ClassFactoryForTestCase >> defaultTagPostfix [

	^ #Default
]

{ #category : #cleaning }
ClassFactoryForTestCase >> delete: aBehavior [

	aBehavior isObsolete ifTrue: [ ^ self ].
	self class environment
		at: #ChangeSet
		ifPresent: [ aBehavior removeFromChanges ].
	(createdSilently includes: aBehavior)
		ifTrue: [ aBehavior removeFromSystemUnlogged ]
		ifFalse: [ aBehavior removeFromSystem ]
]

{ #category : #cleaning }
ClassFactoryForTestCase >> deleteClass: aClass [

	self createdClasses remove: aClass.
	self delete: aClass
]

{ #category : #cleaning }
ClassFactoryForTestCase >> deleteClasses [

	self createdClasses do: [ :class | self delete: class ]
]

{ #category : #cleaning }
ClassFactoryForTestCase >> deletePackage [

	self organization
		removeCategoriesMatching: self packageName , '-*';
		removeEmptyPackagesAndTags
]

{ #category : #cleaning }
ClassFactoryForTestCase >> deleteTraits [

	self createdTraits do: [ :trait | self delete: trait ]
]

{ #category : #accessing }
ClassFactoryForTestCase >> environment [

	^ environment ifNil: [ self class environment ]
]

{ #category : #accessing }
ClassFactoryForTestCase >> environment: anObject [

	environment := anObject
]

{ #category : #initialization }
ClassFactoryForTestCase >> initialize [

	super initialize.
	createdClasses := IdentitySet new.
	createdTraits := IdentitySet new.
	createdSilently := IdentitySet new
]

{ #category : #creating }
ClassFactoryForTestCase >> make: aBlock [
	"I return a new class or trait in the environment of the factory configured as the user specified in the make block.
	Once the test is finished, I'll remove the created class or trait."

	| newClass |
	newClass := self class classInstaller make: [ :aBuilder | "Let's but some default values."
		            aBuilder
			            name: self newClassName;
			            superclass: self defaultSuperclass;
			            installingEnvironment: self environment;
			            package: self packageName.

		            "Now we let the users specify what they want."
		            aBlock value: aBuilder ].

	self createdClasses add: newClass.
	^ newClass
]

{ #category : #creating }
ClassFactoryForTestCase >> newClass [

	^ self make: [ :aBuilder | "We customize nothing on this one" ]
]

{ #category : #creating }
ClassFactoryForTestCase >> newClassName [

	| postFix |
	postFix := self nextCount printString.
	^ (#ClassForTestToBeDeleted , postFix) asSymbol
]

{ #category : #creating }
ClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [

	^ self newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: '' category: category
]

{ #category : #creating }
ClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: poolNamesString category: category [

	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  traitComposition: aTraitComposition;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  sharedPools: poolNamesString;
			  package: self packageName;
			  tag: category ]
]

{ #category : #creating }
ClassFactoryForTestCase >> newTrait [

	^ self
		  newTraitNamed: self newTraitName
		  uses: Array new
		  tag: self defaultTagPostfix
]

{ #category : #creating }
ClassFactoryForTestCase >> newTraitInTag: aTag [

	^ self
		  newTraitNamed: self newTraitName
		  uses: Array new
		  tag: aTag asSymbol
]

{ #category : #creating }
ClassFactoryForTestCase >> newTraitName [

	| postFix |
	postFix := (self createdTraits size + 1) printString.
	^ (#TraitForTestToBeDeleted , postFix) asSymbol
]

{ #category : #creating }
ClassFactoryForTestCase >> newTraitNamed: aTraitName uses: aTraitComposition tag: aTag [

	| newTrait |
	newTrait := self class classInstaller make: [ :aBuilder |
		            aBuilder
			            name: aTraitName;
			            installingEnvironment: self environment;
			            traitComposition: aTraitComposition;
			            package: self packageName;
			            tag: aTag;
			            beTrait ].

	self createdTraits add: newTrait.
	^ newTrait
]

{ #category : #creating }
ClassFactoryForTestCase >> newTraitUsing: aTraitComposition [

	^ self
		  newTraitNamed: self newTraitName
		  uses: aTraitComposition
		  tag: self defaultTagPostfix
]

{ #category : #accessing }
ClassFactoryForTestCase >> nextCount [
	"Global counter to avoid name clash between test runs, in case of some previous failure."

	^ Counter := (Counter ifNil: [ 0 ]) + 1
]

{ #category : #accessing }
ClassFactoryForTestCase >> organization [
	^ self environment organization
]

{ #category : #accessing }
ClassFactoryForTestCase >> packageName [

	^ #CategoryForTestToBeDeleted
]

{ #category : #creating }
ClassFactoryForTestCase >> redefineClass: aClass category: aString [

	^ self
		  redefineClass: aClass
		  subclassOf: aClass superclass
		  uses: aClass traitComposition
		  instanceVariableNames: aClass instanceVariablesString
		  classVariableNames: aClass classVariablesString
		  poolDictionaries: aClass sharedPoolsString
		  package: aString
]

{ #category : #creating }
ClassFactoryForTestCase >> redefineClass: aClass instanceVariableNames: aString [

	^ self
		  redefineClass: aClass
		  subclassOf: aClass superclass
		  uses: aClass traitComposition
		  instanceVariableNames: aString
		  classVariableNames: aClass classVariablesString
		  poolDictionaries: aClass sharedPoolsString
		  package: aClass category
]

{ #category : #creating }
ClassFactoryForTestCase >> redefineClass: aClass subclassOf: aSuperclass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: poolNamesString package: aPackageName [

	| newClass |
	newClass := self class classInstaller make: [ :aBuilder |
		            aBuilder
			            name: aClass name;
			            superclass: aSuperclass;
			            installingEnvironment: self environment;
			            traitComposition: aTraitComposition;
			            slotsFromString: ivNamesString;
			            sharedVariablesFromString: classVarsString;
			            sharedPools: poolNamesString;
			            package: aPackageName asSymbol ].

	self createdClasses add: newClass.
	^ newClass
]

{ #category : #'creating - silently' }
ClassFactoryForTestCase >> silentlyNewClassInCategory: category [

	^ self
		  silentlyNewSubclassOf: self defaultSuperclass
		  instanceVariableNames: ''
		  classVariableNames: ''
		  category: category
]

{ #category : #'creating - silently' }
ClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString [

	^ self
		silentlyNewSubclassOf: aClass
		instanceVariableNames: ivNamesString
		classVariableNames: classVarsString
		category: self defaultTagPostfix
]

{ #category : #'creating - silently' }
ClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [

	^ self
		  silentlyNewSubclassOf: aClass
		  instanceVariableNames: ivNamesString
		  classVariableNames: classVarsString
		  package: (self packageName , '-' , category) asSymbol
]

{ #category : #'creating - silently' }
ClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString package: packageName [

	SystemAnnouncer uniqueInstance suspendAllWhile: [
		^ self
			  newSubclassOf: aClass
			  uses: #(  )
			  instanceVariableNames: ivNamesString
			  classVariableNames: classVarsString
			  poolDictionaries: ''
			  category: packageName ]
]
