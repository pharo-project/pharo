"
Unit test for trait packaging
"
Class {
	#name : 'T2TraitWithPackagesTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'instance creation' }
T2TraitWithPackagesTest >> newTrait: aName with: slots traits: aComposition package: package [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  traitComposition: aComposition;
			  slots: slots;
			  package: package;
			  beTrait ]
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> packageNameForTests2 [

	^ self packageNameForTests , '2'
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> packageUnderTest [

	^ self packageOrganizer packageNamed: self packageNameForTests
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> packageUnderTest2 [

	^ self packageOrganizer packageNamed: self packageNameForTests2
]

{ #category : 'running' }
T2TraitWithPackagesTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests2.

	super tearDown
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraits [
	| t1 t2 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreation [
	| t1 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreationOverriden [
	| t1 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverriden [
	| t1 t2 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^27.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenAndRemoved [
	| t1 t2 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^27.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2.

	t2 removeSelector: #m1.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenModifiedKeepPackage [
	| t1 t2 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^27.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2.

	t2 compile: 'm1 ^26.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsRemoved [

	| t1 t2 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	t2 := self
		      newTrait: #T2
		      with: #(  )
		      traits: t1
		      package: self packageNameForTests2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	self packageUnderTest includesSelector: #m1 ofClass: t1.

	t1 removeSelector: #m1.

	self assert: (t1 protocolOfSelector: #m1) isNil.
	self assert: (t2 protocolOfSelector: #m1) isNil.

	self packageOrganizer packages do: [ :package |
		self deny: (package includesSelector: #m1 ofClass: t1).
		self deny: (package includesSelector: #m1 ofClass: t2) ]
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfRemovedTrait [

	| t1 t2 |
	t1 := self newTrait: #T1.
	t2 := self
		      newTrait: #T2
		      with: #(  )
		      traits: t1
		      package: self packageNameForTests2.

	self assert: t1 package equals: self packageUnderTest.
	self assert: t2 package equals: self packageUnderTest2.

	self assert: (self packageOrganizer packageOfClassNamed: #T1) equals: self packageUnderTest.
	self assert: (self packageOrganizer packageOfClassNamed: #T2) equals: self packageUnderTest2.

	t2 removeFromSystem.

	self assert: (self packageOrganizer packageOfClassNamed: #T1) equals: self packageUnderTest.
	self assert: (self packageOrganizer packageOfClassNamed: #T2) equals: self packageOrganizer undefinedPackage.

	t1 removeFromSystem.

	self assert: (self packageOrganizer packageOfClassNamed: #T1) equals: self packageOrganizer undefinedPackage.
	self assert: (self packageOrganizer packageOfClassNamed: #T2) equals: self packageOrganizer undefinedPackage
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testTraitMethodPackage [

	| t1 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testTraitPackage [

	| t1 |
	t1 := self newTrait: #T1.

	self assert: t1 package equals: self packageUnderTest
]
