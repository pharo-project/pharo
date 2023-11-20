"
Unit test for trait packaging
"
Class {
	#name : 'TraitWithPackagesTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'instance creation' }
TraitWithPackagesTest >> newTrait: aName with: slots traits: aComposition package: package [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  traitComposition: aComposition;
			  slots: slots;
			  package: package;
			  beTrait ]
]

{ #category : 'tests' }
TraitWithPackagesTest >> packageNameForTests2 [

	^ self packageNameForTests , '2'
]

{ #category : 'tests' }
TraitWithPackagesTest >> packageUnderTest [

	^ self packageOrganizer packageNamed: self packageNameForTests
]

{ #category : 'tests' }
TraitWithPackagesTest >> packageUnderTest2 [

	^ self packageOrganizer packageNamed: self packageNameForTests2
]

{ #category : 'running' }
TraitWithPackagesTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests2.

	super tearDown
]

{ #category : 'tests' }
TraitWithPackagesTest >> testPackageOfMethodFromTraits [
	| t1 t2 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : 'tests' }
TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreation [
	| t1 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2 with: #() traits: t1 package: self packageNameForTests2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : 'tests' }
TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreationOverriden [
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
TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverriden [
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
TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenAndRemoved [
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
TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenModifiedKeepPackage [
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
TraitWithPackagesTest >> testPackageOfMethodFromTraitsRemoved [

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
TraitWithPackagesTest >> testPackageOfRemovedTrait [

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
TraitWithPackagesTest >> testTraitMethodPackage [

	| t1 |
	t1 := self newTrait: #T1.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest
]

{ #category : 'tests' }
TraitWithPackagesTest >> testTraitPackage [

	| t1 |
	t1 := self newTrait: #T1.

	self assert: t1 package equals: self packageUnderTest
]
