"
Abstract Test of all the Traits tests
"
Class {
	#name : 'TraitAbstractTest',
	#superclass : 'TestCase',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'testing' }
TraitAbstractTest class >> isAbstract [

	^self == TraitAbstractTest
]

{ #category : 'instance creation' }
TraitAbstractTest >> newClass: aName [
	^self newClass: aName with: #() traits: TEmpty
]

{ #category : 'utilities' }
TraitAbstractTest >> newClass: aSymbol superclass: aClass traits: aTraitComposition [

	^ self newClass: aSymbol superclass: aClass with: {  } traits: aTraitComposition
]

{ #category : 'instance creation' }
TraitAbstractTest >> newClass: aName superclass: aSuperclass with: slots traits: aComposition [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  superclass: aSuperclass;
			  slots: slots;
			  traitComposition: aComposition;
			  package: self packageNameForTests ]
]

{ #category : 'instance creation' }
TraitAbstractTest >> newClass: aName traits: aComposition [

	^ self newClass: aName with: {  } traits: aComposition
]

{ #category : 'instance creation' }
TraitAbstractTest >> newClass: aName with: slots traits: aComposition [
	^ self newClass: aName superclass: Object with: slots traits: aComposition
]

{ #category : 'instance creation' }
TraitAbstractTest >> newTrait: aName [
	^ self newTrait: aName with: #()
]

{ #category : 'instance creation' }
TraitAbstractTest >> newTrait: aName traits: traits [

	^ self newTrait: aName with: #(  ) traits: traits
]

{ #category : 'instance creation' }
TraitAbstractTest >> newTrait: aName with: slots [
	^ self newTrait: aName with: slots traits: TEmpty
]

{ #category : 'instance creation' }
TraitAbstractTest >> newTrait: aName with: slots traits: aComposition [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  traitComposition: aComposition;
			  slots: slots;
			  package: self packageNameForTests;
			  beTrait ]
]

{ #category : 'setup' }
TraitAbstractTest >> packageNameForTests [

	^ #'Generated-Trait-Test-Package'
]

{ #category : 'running' }
TraitAbstractTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.

	super tearDown
]
