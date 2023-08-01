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
	#category : #'Fuel-Core-Tests-Support-ClassFactory'
}

{ #category : #'accessing-defaults' }
FLClassFactoryForTestCase >> defaultPackageAndTag [
	^ (self packageName , '-', self defaultTagPostfix) asSymbol
]

{ #category : #cleaning }
FLClassFactoryForTestCase >> delete: aBehavior [

	createdBehaviors remove: aBehavior ifAbsent: [  ].
	aBehavior isObsolete ifTrue: [ ^ self ].

	self class environment at: #ChangeSet ifPresent: [ :changeSet | aBehavior removeFromChanges ].

	aBehavior removeFromSystemUnlogged
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
FLClassFactoryForTestCase >> newClassWithInstanceVariableNames: aString [

	^ self
		  newSubclassOf: self defaultSuperclass
		  uses: {  }
		  instanceVariableNames: aString
		  classVariableNames: ''
]

{ #category : #creating }
FLClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString [

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
			            package: self defaultPackageAndTag ].

	createdBehaviors add: newClass.
	^ newClass
]

{ #category : #'creating-traits' }
FLClassFactoryForTestCase >> newTrait [

	^ self newTraitNamed: self newBehaviorName instanceVariableNames: ''
]

{ #category : #'creating-traits' }
FLClassFactoryForTestCase >> newTraitNamed: aTraitName instanceVariableNames: aString [

	| newTrait |
	newTrait := self class classInstaller make: [ :aBuilder |
		            aBuilder
			            environment: self environment;
			            name: aTraitName;
			            slots: aString asSlotCollection;
			            package: self defaultPackageAndTag;
			            beTrait ].

	createdBehaviors add: newTrait.
	^ newTrait
]

{ #category : #accessing }
FLClassFactoryForTestCase >> registerBehavior: aBehavior [

	createdBehaviors add: aBehavior
]

{ #category : #'compiling-silently' }
FLClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior [

	^ self silentlyCompile: aString in: aBehavior storingSource: true
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
FLClassFactoryForTestCase >> silentlyNewClassWithInstanceVariableNames: aString [

	^ self silentlyNewSubclassOf: self defaultSuperclass instanceVariableNames: aString
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: aString [

	^ self silentlyNewSubclassOf: aClass instanceVariableNames: aString classVariableNames: ''
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString [

	^ self
		  silentlyNewSubclassOf: aClass
		  uses: #(  )
		  instanceVariableNames: ivNamesString
		  classVariableNames: classVarsString
]

{ #category : #'creating-silently' }
FLClassFactoryForTestCase >> silentlyNewSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString [

	^ self silentlyDo: [
		  self
			  newSubclassOf: aClass
			  uses: aTraitComposition
			  instanceVariableNames: ivNamesString
			  classVariableNames: classVarsString ]
]

{ #category : #'creating-traits-silently' }
FLClassFactoryForTestCase >> silentlyNewTrait [

	^ self silentlyNewTraitNamed: self newBehaviorName instanceVariableNames: ''
]

{ #category : #'creating-traits-silently' }
FLClassFactoryForTestCase >> silentlyNewTraitNamed: aTraitName instanceVariableNames: aString [

	^ self silentlyDo: [ self newTraitNamed: aTraitName instanceVariableNames: aString ]
]

{ #category : #'creating-traits-silently' }
FLClassFactoryForTestCase >> silentlyNewTraitWithInstanceVariableNames: aString [

	^ self silentlyNewTraitNamed: self newBehaviorName instanceVariableNames: aString
]

{ #category : #'renaming-silently' }
FLClassFactoryForTestCase >> silentlyRename: aClass to: aName [
	^ self silentlyDo: [ aClass rename: aName asSymbol ]
]
