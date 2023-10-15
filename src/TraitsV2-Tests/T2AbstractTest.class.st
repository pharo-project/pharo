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

{ #category : 'instance creation' }
T2AbstractTest >> make: aBlock [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder package: self packageNameForTests.
		  aBlock value: aBuilder ]
]

{ #category : 'instance creation' }
T2AbstractTest >> makeTrait: aBlock [

	^ self make: [ :aBuilder |
		  aBuilder beTrait.
		  aBlock value: aBuilder ]
]

{ #category : 'instance creation' }
T2AbstractTest >> newClass: aName superclass: aSuperclass with: slots uses: aComposition [

	^ self make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  superclass: aSuperclass;
			  slots: slots;
			  traitComposition: aComposition ]
]

{ #category : 'instance creation' }
T2AbstractTest >> newClass: aName uses: aComposition [

	^ self make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  traitComposition: aComposition ]
]

{ #category : 'instance creation' }
T2AbstractTest >> newClass: aName with: slots [

	^ self make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  slots: slots ]
]

{ #category : 'instance creation' }
T2AbstractTest >> newClass: aName with: slots uses: aComposition [

	^ self make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  slots: slots;
			  traitComposition: aComposition ]
]

{ #category : 'instance creation' }
T2AbstractTest >> newTrait: aName with: slots [
	^ self newTrait: aName with: slots uses: TEmpty
]

{ #category : 'instance creation' }
T2AbstractTest >> newTrait: aName with: slots uses: aComposition [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  traitComposition: aComposition;
			  slots: slots;
			  package: self packageNameForTests;
			  beTrait ]
]

{ #category : 'instance creation' }
T2AbstractTest >> newTrait: aName with: slots uses: aComposition category: category [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  traitComposition: aComposition;
			  slots: slots;
			  package: category;
			  beTrait ]
]

{ #category : 'instance creation' }
T2AbstractTest >> packageNameForTests [

	^ #'Generated-Trait-Test-Package'
]

{ #category : 'running' }
T2AbstractTest >> tearDown [
	self packageOrganizer removePackage: self packageNameForTests.

	super tearDown
]
