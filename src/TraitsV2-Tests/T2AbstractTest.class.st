"
Abstract Test of all the TraitsV2 tests
"
Class {
	#name : 'T2AbstractTest',
	#superclass : 'TestCase',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'testing' }
T2AbstractTest class >> isAbstract [

	^self == T2AbstractTest
]

{ #category : 'utilities' }
T2AbstractTest >> classesAndTraitsInTestPackage [

	^ (self packageOrganizer packageNamed: self packageNameForTests) definedClasses
]

{ #category : 'instance creation' }
T2AbstractTest >> newClass: aName [
	^self newClass: aName with: #() traits: TEmpty
]

{ #category : 'utilities' }
T2AbstractTest >> newClass: aSymbol superclass: aClass traits: aTraitComposition [

	^ self newClass: aSymbol superclass: aClass with: {  } traits: aTraitComposition
]

{ #category : 'instance creation' }
T2AbstractTest >> newClass: aName superclass: aSuperclass with: slots traits: aComposition [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  superclass: aSuperclass;
			  slots: slots;
			  traitComposition: aComposition;
			  package: self packageNameForTests ]
]

{ #category : 'instance creation' }
T2AbstractTest >> newClass: aName with: slots traits: aComposition [
	^ self newClass: aName superclass: Object with: slots traits: aComposition
]

{ #category : 'instance creation' }
T2AbstractTest >> newTrait: aName [
	^ self newTrait: aName with: #()
]

{ #category : 'instance creation' }
T2AbstractTest >> newTrait: aName traits: traits [

	^ self newTrait: aName with: #(  ) traits: traits
]

{ #category : 'instance creation' }
T2AbstractTest >> newTrait: aName with: slots [
	^ self newTrait: aName with: slots traits: TEmpty
]

{ #category : 'instance creation' }
T2AbstractTest >> newTrait: aName with: slots traits: aComposition [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  traitComposition: aComposition;
			  slots: slots;
			  package: self packageNameForTests;
			  beTrait ]
]

{ #category : 'setup' }
T2AbstractTest >> packageNameForTests [

	^ #'Generated-Trait-Test-Package'
]

{ #category : 'running' }
T2AbstractTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.

	super tearDown
]
