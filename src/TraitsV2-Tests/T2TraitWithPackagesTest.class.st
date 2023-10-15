"
Unit test for trait packaging
"
Class {
	#name : 'T2TraitWithPackagesTest',
	#superclass : 'T2AbstractTest',
	#instVars : [
		't1'
	],
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'helpers' }
T2TraitWithPackagesTest >> createT2 [

	^ self makeTrait: [ :aBuilder |
		  aBuilder
			  name: #T2;
			  traitComposition: t1;
			  package: self packageNameForTest2 ]
]

{ #category : 'helpers' }
T2TraitWithPackagesTest >> packageForTest [

	^ self packageOrganizer packageNamed: self packageNameForTests
]

{ #category : 'helpers' }
T2TraitWithPackagesTest >> packageForTest2 [

	^ self packageOrganizer packageNamed: self packageNameForTest2
]

{ #category : 'helpers' }
T2TraitWithPackagesTest >> packageNameForTest2 [
	^ 'TestPackage2'
]

{ #category : 'running' }
T2TraitWithPackagesTest >> setUp [

	super setUp.

	t1 := self makeTrait: [ :builder | builder name: #T1 ]
]

{ #category : 'running' }
T2TraitWithPackagesTest >> tearDown [

	(self packageNameForTests asPackageIfAbsent: [ nil ]) ifNotNil: [ :package | package removeFromSystem ].
	(self packageNameForTest2 asPackageIfAbsent: [ nil ]) ifNotNil: [ :package | package removeFromSystem ].

	super tearDown
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraits [

	| t2 |
	t1 compile: 'm1 ^42.'.

	t2 := self createT2.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreation [

	| t2 |
	t2 := self createT2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreationOverriden [

	| t2 |
	t2 := self createT2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest.

	t2 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest2
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverriden [

	| t2 |
	t1 compile: 'm1 ^42.'.

	t2 := self createT2.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest.

	t2 compile: 'm1 ^27.'.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest2
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenAndRemoved [

	| t2 |
	t1 compile: 'm1 ^42.'.

	t2 := self createT2.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest.

	t2 compile: 'm1 ^27.'.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest2.

	t2 removeSelector: #m1.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenModifiedKeepPackage [

	| t2 |
	t1 compile: 'm1 ^42.'.

	t2 := self createT2.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest.

	t2 compile: 'm1 ^27.'.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest2.

	t2 compile: 'm1 ^26.'.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest2
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsRemoved [

	| t2 |
	t1 compile: 'm1 ^42.'.

	t2 := self createT2.

	self assert: (t1 >> #m1) package equals: self packageForTest.
	self assert: (t2 >> #m1) package equals: self packageForTest.

	self packageForTest includesSelector: #m1 ofClass: t1.

	t1 removeSelector: #m1.

	self assert: (t1 protocolOfSelector: #m1) isNil.
	self assert: (t2 protocolOfSelector: #m1) isNil.

	self packageOrganizer packages do: [ :package |
		self deny: (package includesSelector: #m1 ofClass: t1).
		self deny: (package includesSelector: #m1 ofClass: t2) ]
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testPackageOfRemovedTrait [

	| t2 |
	t2 := self createT2.

	self assert: t1 package equals: self packageForTest.
	self assert: t2 package equals: self packageForTest2.

	self assert: (self packageOrganizer packageOfClassNamed: #T1) equals: self packageForTest.
	self assert: (self packageOrganizer packageOfClassNamed: #T2) equals: self packageForTest2.

	t2 removeFromSystem.

	self assert: (self packageOrganizer packageOfClassNamed: #T1) equals: self packageForTest.
	self assert: (self packageOrganizer packageOfClassNamed: #T2) equals: self packageOrganizer undefinedPackage.

	t1 removeFromSystem.

	self assert: (self packageOrganizer packageOfClassNamed: #T1) equals: self packageOrganizer undefinedPackage.
	self assert: (self packageOrganizer packageOfClassNamed: #T2) equals: self packageOrganizer undefinedPackage
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testTraitMethodPackage [

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageForTest
]

{ #category : 'tests' }
T2TraitWithPackagesTest >> testTraitPackage [

	self assert: t1 package equals: self packageForTest
]
