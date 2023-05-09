"
Unit test for trait packaging
"
Class {
	#name : #T2TraitWithPackagesTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitWithPackagesTest >> packageName [
	^ 'TestPackage'
]

{ #category : #tests }
T2TraitWithPackagesTest >> packageName2 [
	^ 'TestPackage2'
]

{ #category : #tests }
T2TraitWithPackagesTest >> packageUnderTest [

	^ Smalltalk organization packageNamed: self packageName
]

{ #category : #tests }
T2TraitWithPackagesTest >> packageUnderTest2 [

	^ Smalltalk organization packageNamed: self packageName2
]

{ #category : #tests }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraits [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.

	t1 compile: 'm1 ^42.'.

	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : #tests }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreation [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : #tests }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsAfterCreationOverriden [
	| t1 t2 |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	t2 := self newTrait: #T2 with: #() uses: t1 category: self packageName2.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	t2 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest2
]

{ #category : #tests }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverriden [
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
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenAndRemoved [
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
	self assert: (t2 >> #m1) package equals: self packageUnderTest
]

{ #category : #tests }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsOverridenModifiedKeepPackage [
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
	self assert: (t2 >> #m1) package equals: self packageUnderTest2
]

{ #category : #tests }
T2TraitWithPackagesTest >> testPackageOfMethodFromTraitsRemoved [

	| t1 t2 |
	t1 := self
		      newTrait: #T1
		      with: #(  )
		      uses: {  }
		      category: self packageName.

	t1 compile: 'm1 ^42.'.

	t2 := self
		      newTrait: #T2
		      with: #(  )
		      uses: t1
		      category: self packageName2.

	self assert: (t1 >> #m1) package equals: self packageUnderTest.
	self assert: (t2 >> #m1) package equals: self packageUnderTest.

	self assert: (Smalltalk organization packageDefiningOrExtendingSelector: #m1 inClassNamed: #T1) equals: self packageUnderTest.

	t1 removeSelector: #m1.

	self assert: (t1 organization protocolOfSelector: #m1) isNil.
	self assert: (t2 organization protocolOfSelector: #m1) isNil.

	self assert: (Smalltalk organization packageDefiningOrExtendingSelector: #m1 inClassNamed: #T1) isNil.
	self assert: (Smalltalk organization packageDefiningOrExtendingSelector: #m1 inClassNamed: #T2) isNil
]

{ #category : #tests }
T2TraitWithPackagesTest >> testPackageOfRemovedTrait [

	| t1 t2 |
	t1 := self
		      newTrait: #T1
		      with: #(  )
		      uses: {  }
		      category: self packageName.
	t2 := self
		      newTrait: #T2
		      with: #(  )
		      uses: t1
		      category: self packageName2.

	self assert: t1 package equals: self packageUnderTest.
	self assert: t2 package equals: self packageUnderTest2.

	self assert: (Smalltalk organization packageOfClassNamed: #T1) equals: self packageUnderTest.
	self assert: (Smalltalk organization packageOfClassNamed: #T2) equals: self packageUnderTest2.

	t2 removeFromSystem.

	self assert: (Smalltalk organization packageOfClassNamed: #T1) equals: self packageUnderTest.
	self assert: (Smalltalk organization packageOfClassNamed: #T2) isNil.

	t1 removeFromSystem.

	self assert: (Smalltalk organization packageOfClassNamed: #T1) isNil.
	self assert: (Smalltalk organization packageOfClassNamed: #T2) isNil
]

{ #category : #tests }
T2TraitWithPackagesTest >> testTraitMethodPackage [
	| t1 testPackage |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	testPackage := self packageUnderTest.

	t1 compile: 'm1 ^42.'.

	self assert: (t1 >> #m1) package equals: testPackage
]

{ #category : #tests }
T2TraitWithPackagesTest >> testTraitPackage [
	| t1 testPackage |
	t1 := self newTrait: #T1 with: #() uses: {} category: self packageName.
	testPackage := self packageUnderTest.

	self assert: t1 package equals: testPackage
]
