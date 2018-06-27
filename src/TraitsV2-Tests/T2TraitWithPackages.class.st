"
Unit test for trait packaging
"
Class {
	#name : #T2TraitWithPackages,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests-Tests'
}

{ #category : #tests }
T2TraitWithPackages >> packageName [
	^ 'TestPackage'
]

{ #category : #tests }
T2TraitWithPackages >> packageName2 [
	^ 'TestPackage2'
]

{ #category : #tests }
T2TraitWithPackages >> packageUnderTest [
	^ RPackageOrganizer default packageNamed: self packageName
]

{ #category : #tests }
T2TraitWithPackages >> packageUnderTest2 [
	^ RPackageOrganizer default packageNamed: self packageName2
]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfMethodFromTraits [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
	
]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfMethodFromTraitsAfterCreation [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest	
	
]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfMethodFromTraitsAfterCreationOverriden [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2.
	
]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfMethodFromTraitsOverriden [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^27.'.
	
	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2

]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfMethodFromTraitsOverridenAndRemoved [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^27.'.
	
	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2.

	t2 removeSelector: #m1.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfMethodFromTraitsOverridenModifiedKeepPackage [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^27.'.
	
	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2.

	t2 compile: 'm1 ^26.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2.

]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfMethodFromTraitsRemoved [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	self assert: (RPackageOrganizer default packageDefiningOrExtendingSelector: #m1 inClassNamed: #T1) equals: self packageUnderTest.

	t1 removeSelector: #m1.
	
	self assert: (t1 organization categoryOfElement: #m1) isNil.
	self assert: (t2 organization categoryOfElement: #m1) isNil.	
	
	self assert: (RPackageOrganizer default packageDefiningOrExtendingSelector: #m1 inClassNamed: #T1) isNil.
	self assert: (RPackageOrganizer default packageDefiningOrExtendingSelector: #m1 inClassNamed: #T2) isNil.	
]

{ #category : #tests }
T2TraitWithPackages >> testPackageOfRemovedTrait [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	self assert: t1 package equals: self packageUnderTest.
	self assert: t2 package equals: self packageUnderTest2.

	self assert: (RPackageOrganizer default packageOfClassNamed: #T1) equals: self packageUnderTest.
	self assert: (RPackageOrganizer default packageOfClassNamed: #T2) equals: self packageUnderTest2.

	t2 removeFromSystem.

	self assert: (RPackageOrganizer default packageOfClassNamed: #T1) equals: self packageUnderTest.
	self assert: (RPackageOrganizer default packageOfClassNamed: #T2) isNil.
	
	t1 removeFromSystem.
	
	self assert: (RPackageOrganizer default packageOfClassNamed: #T1) isNil.
	self assert: (RPackageOrganizer default packageOfClassNamed: #T2) isNil.

]

{ #category : #tests }
T2TraitWithPackages >> testTraitMethodPackage [
	| t1 testPackage |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	testPackage := self packageUnderTest.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: testPackage
]

{ #category : #tests }
T2TraitWithPackages >> testTraitPackage [
	| t1 testPackage |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	testPackage := self packageUnderTest.

	self assert: t1 package equals: testPackage
]
