"
SUnit tests for Package
"
Class {
	#name : 'PackageAndTraitOnModelTest',
	#superclass : 'PackageTestCase',
	#instVars : [
		'a1',
		't1',
		't2',
		'xPackage',
		'yPackage',
		'zPackage'
	],
	#category : 'Kernel-Packages-Tests',
	#package : 'Kernel-Packages-Tests'
}

{ #category : 'running' }
PackageAndTraitOnModelTest >> setUp [

	super setUp.

	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	a1 := self newClassNamed: #A1DefinedInX in: xPackage.

	"a1 defines two normal = local methods"
	a1 compile: 'localMethodDefinedInP1 ^ #methodDefinedInP1'.
	a1 compile: 'anotherLocalMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.

	t1 := self newTraitNamed: #TraitInPackageP1 in: xPackage.
	t1 compile: 'traitMethodDefinedInP1 ^ #traitMethodDefinedInP1'.

	"P3 defines a new method extension on T1 (packaged in p1)"
	t1 compile: 'traitMethodExtendedFromP3 ^ #traitMethodExtendedFromP3' classified: '*' , self zPackageName.

	t2 := self newTraitNamed: #TraitInPackageP2 in: yPackage.
	t2 compile: 'traitMethodDefinedInP2 ^ #traitMethodDefinedInP2'.

	"Here P1 extends T2 from P2 with a new method"
	t2 compile: 'traitMethodExtendedFromP1 ^ #traitMethodExtendedFromP1' classified: '*' , self xPackageName.

	a1 setTraitComposition: (t1 + t2) asTraitComposition
]

{ #category : 'tests' }
PackageAndTraitOnModelTest >> testMethodOverridingTraitMethodIsKnowByPackage [
	"Regression test for a bug where adding a method to a class that overrides a method from a trait was not know by the package of the class"

	| a2 |
	a2 := self newClassNamed: #A2DefinedInX in: xPackage.

	a2 setTraitComposition: t1 asTraitComposition.

	self deny: (xPackage includesSelector: #traitMethodDefinedInP1 ofClass: a2).
	self assert: (a2 >> #traitMethodDefinedInP1) isFromTrait.

	a2 compile: 'traitMethodDefinedInP1 "Override in the class"'.

	self assert: (a2 >> #traitMethodDefinedInP1) package equals: xPackage.
	self deny: (a2 >> #traitMethodDefinedInP1) isFromTrait
]

{ #category : 'tests' }
PackageAndTraitOnModelTest >> testPackageOfClassMethodFromTraitExtensionIsExtendingPackage [
	"The package of a method defined in atrait but package in another package than the extended trait is the
	package containing the extension."

	"The package of a method in A1 (which is coming from the trait T1 used by A1) is the package of T1"

	self assert: (a1 >> #traitMethodExtendedFromP1) package equals: xPackage.
	"The package of a method in A1 (which is coming from the trait T1 used by A1 but extended in package T2) is the package of T2"
	self assert: (a1 >> #traitMethodExtendedFromP3) package equals: zPackage
]

{ #category : 'tests' }
PackageAndTraitOnModelTest >> testPackageOfClassMethodFromTraitIsTraitPackage [
	"test that a class method coming from a trait is packaged in the trait package"

	self assert: (a1 >> #traitMethodDefinedInP1) package equals: xPackage.
	self assert: (a1 >> #traitMethodDefinedInP2) package equals: yPackage
]

{ #category : 'tests' }
PackageAndTraitOnModelTest >> testPackageOfClassMethodIsClassPackage [
	"The package of a local method (not defined in a trait) is the package of its class"

	self assert: (a1 >> #localMethodDefinedInP1) package equals: xPackage.
	self assert: (a1 >> #anotherLocalMethodDefinedInP1) package equals: xPackage.
	self assert: (a1 >> #anotherLocalMethodDefinedInP1) package equals: xPackage
]

{ #category : 'tests' }
PackageAndTraitOnModelTest >> testPackageOfTraitMethodIsTraitPackage [
	"The package of a trait method is the package of its trait."

	self assert: (a1 >> #traitMethodDefinedInP2) package equals: yPackage.
	self assert: (a1 >> #traitMethodDefinedInP2) package equals: yPackage.
	self assert: (a1 >> #traitMethodDefinedInP1) package equals: xPackage
]

{ #category : 'tests' }
PackageAndTraitOnModelTest >> testStartingSituation [

	self deny: (yPackage includesClass: a1).
	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: t1).
	self assert: (xPackage definesOrExtendsClass: a1).
	self assert: (yPackage includesClass: t2)
]
