"
I'm useful when classes needs to be created during the execution of the test. This avoid polluting your unit tests with dummy and mock classes.
A typical usage of it is:

```
TestCase << #YourTest
    slots: { #classFactory};
	  package: 'YourPackage'
```
```
YourTest >> setUp
	  super setUp. 
    classFactory := ClassFactoryForTestCase new
```
```
YourTest >> tearDown
    classFactory deleteClasses.
    super tearDown 
```

```
YourTest >> testIsBehavior
    | cls |
    cls := classFactory newClass.
    self assert: cls isBehavior
```
"
Class {
	#name : 'ClassFactoryForTestCase',
	#superclass : 'Object',
	#instVars : [
		'createdBehaviors',
		'createdSilently',
		'environment'
	],
	#classVars : [
		'Counter'
	],
	#category : 'SUnit-Core-Extensions',
	#package : 'SUnit-Core',
	#tag : 'Extensions'
}

{ #category : 'instance creation' }
ClassFactoryForTestCase class >> environment: aSystemEnvironment [

	^ self new
		  environment: aSystemEnvironment;
		  yourself
]

{ #category : 'cleaning' }
ClassFactoryForTestCase >> cleanUp [

	self createdBehaviors copy do: [ :behavior | self delete: behavior ].
	self deletePackage
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> createdBehaviorNames [

	^ self createdBehaviors collect: [ :class | class name ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> createdBehaviors [

	^ createdBehaviors
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> createdClassNames [

	^ self createdClasses collect: [ :class | class name ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> createdClasses [

	^ self createdBehaviors reject: [ :behavior | behavior isTrait ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> createdSilently [

	^ createdSilently
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> createdTraitNames [

	^ self createdTraits collect: [ :trait | trait name ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> createdTraits [

	^ self createdBehaviors select: [ :behavior | behavior isTrait ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> defaultSuperclass [

	^ Object
]

{ #category : 'cleaning' }
ClassFactoryForTestCase >> delete: aBehavior [

	| name |
	createdBehaviors remove: aBehavior ifAbsent: [  ].
	aBehavior isObsolete ifTrue: [ ^ self ].
	name := aBehavior name. "save it as it will be obsolete later"
	(createdSilently includes: aBehavior)
		ifTrue: [
			createdSilently remove: aBehavior.
			aBehavior removeFromSystemUnlogged ]
		ifFalse: [ aBehavior removeFromSystem ].
	"We know that we can remove the key from the undeclared registry, as it was added by #removeFromSystem"
	self class undeclaredRegistry removeKey: name ifAbsent: [ "might be an anonymous class" ]
]

{ #category : 'cleaning' }
ClassFactoryForTestCase >> deletePackage [

	(self organization packages select: [ :package | package name beginsWith: self packageName ]) do: [ :package | package removeFromSystem ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> environment [

	^ environment ifNil: [ self class environment ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> environment: anObject [

	environment := anObject
]

{ #category : 'initialization' }
ClassFactoryForTestCase >> initialize [

	super initialize.
	createdBehaviors := IdentitySet new.
	createdSilently := IdentitySet new
]

{ #category : 'creating' }
ClassFactoryForTestCase >> make: aBlock [
	"I return a new class or trait in the environment of the factory configured as the user specified in the make block.
	Once the test is finished, I'll remove the created class or trait."

	| newClass |
	newClass := self class classInstaller make: [ :aBuilder | "Let's but some default values."
		            aBuilder
			            name: self newBehaviorName;
			            superclass: self defaultSuperclass;
			            installingEnvironment: self environment;
			            package: self packageName.

		            "Now we let the users specify what they want."
		            aBlock value: aBuilder ].

	self registerBehavior: newClass.
	^ newClass
]

{ #category : 'creating' }
ClassFactoryForTestCase >> newAnonymousClass [
	^ self defaultSuperclass newAnonymousSubclass
]

{ #category : 'creating' }
ClassFactoryForTestCase >> newAnonymousTrait [
	^ Smalltalk anonymousClassInstaller make: [ :builder |
		builder
			"The name is necessary to not break Pharo"
			name: self newBehaviorName;
			beTrait ]
]

{ #category : 'creating' }
ClassFactoryForTestCase >> newBehaviorName [

	^ (#BehaviorForTestToBeDeleted , self nextCount printString) asSymbol
]

{ #category : 'creating' }
ClassFactoryForTestCase >> newClass [

	^ self make: [ :aBuilder | "We customize nothing on this one" ]
]

{ #category : 'creating' }
ClassFactoryForTestCase >> newTrait [

	^ self make: [ :aBuilder | aBuilder beTrait ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> nextCount [
	"Global counter to avoid name clash between test runs, in case of some previous failure."

	^ Counter := (Counter ifNil: [ 0 ]) + 1
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> organization [
	^ self environment organization
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> packageName [

	^ #CategoryForTestToBeDeleted
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> registerBehavior: aBehavior [

	createdBehaviors add: aBehavior
]

{ #category : 'compiling - silently' }
ClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior [

	^ self silentlyCompile: aString in: aBehavior storingSource: true
]

{ #category : 'compiling - silently' }
ClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior protocol: anotherString [

	^ self
		  silentlyCompile: aString
		  in: aBehavior
		  protocol: anotherString
		  storingSource: true
]

{ #category : 'compiling - silently' }
ClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior protocol: anotherString storingSource: aBoolean [

	^ self silentlyDo: [
		  aBehavior
			  compile: aString
			  classified: anotherString
			  withStamp: nil
			  notifying: nil
			  logSource: aBoolean ]
]

{ #category : 'compiling - silently' }
ClassFactoryForTestCase >> silentlyCompile: aString in: aBehavior storingSource: aBoolean [

	^ self
		  silentlyCompile: aString
		  in: aBehavior
		  protocol: 'unclassified'
		  storingSource: aBoolean
]

{ #category : 'private' }
ClassFactoryForTestCase >> silentlyDo: aBlock [
	^ SystemAnnouncer uniqueInstance suspendAllWhile: aBlock
]

{ #category : 'creating' }
ClassFactoryForTestCase >> silentlyMake: aBlock [

	| behavior |
	behavior := self silentlyDo: [ self make: aBlock ].

	createdSilently add: behavior.
	^ behavior
]

{ #category : 'creating' }
ClassFactoryForTestCase >> silentlyNewClass [

	^ self silentlyMake: [ :aBuilder | "Nothing to configure here" ]
]

{ #category : 'creating' }
ClassFactoryForTestCase >> silentlyNewTrait [

	^ self silentlyMake: [ :aBuilder | aBuilder beTrait ]
]

{ #category : 'creating' }
ClassFactoryForTestCase >> silentlyRename: aClass to: aName [
	^ self silentlyDo: [ aClass rename: aName asSymbol ]
]

{ #category : 'accessing' }
ClassFactoryForTestCase >> tagName [

	^ #Default
]

{ #category : 'creating' }
ClassFactoryForTestCase >> update: aClass to: aBlock [
	"I return an updated class or trait in the environment of the factory configured as the user specified in the make block.
	Once the test is finished, I'll remove the created class or trait."

	| newClass |
	newClass := self class classInstaller update: aClass to: [ :aBuilder | "Let's but some default values."
		            aBuilder
			            installingEnvironment: self environment;
			            package: self packageName.

		            "Now we let the users specify what they want."
		            aBlock value: aBuilder ].

	self registerBehavior: newClass.
	^ newClass
]
