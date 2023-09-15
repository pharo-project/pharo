"
SUnit tests for RPackage
"
Class {
	#name : #RPackageTraitTest,
	#superclass : #RPackageIncrementalTest,
	#instVars : [
		'a1',
		't1',
		't2',
		'p1',
		'p2',
		'p3'
	],
	#category : #'RPackage-Tests'
}

{ #category : #running }
RPackageTraitTest >> setUp [

	super setUp.

	p1 := self organizer ensurePackage: self p1Name.
	p2 := self organizer ensurePackage: self p2Name.
	p3 := self organizer ensurePackage: self p3Name.

	a1 := self newClassNamed: #A1DefinedInP1 in: p1.

	"a1 defines two normal = local methods"
	a1 compile: 'localMethodDefinedInP1 ^ #methodDefinedInP1'.
	a1 compile: 'anotherLocalMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.

	t1 := self newTraitNamed: #TraitInPackageP1 in: p1.
	t1 compile: 'traitMethodDefinedInP1 ^ #traitMethodDefinedInP1'.

	"P3 defines a new method extension on T1 (packaged in p1)"
	t1 compile: 'traitMethodExtendedFromP3 ^ #traitMethodExtendedFromP3' classified: '*' , self p3Name.

	t2 := self newTraitNamed: #TraitInPackageP2 in: p2.
	t2 compile: 'traitMethodDefinedInP2 ^ #traitMethodDefinedInP2'.

	"Here P1 extends T2 from P2 with a new method"
	t2 compile: 'traitMethodExtendedFromP1 ^ #traitMethodExtendedFromP1' classified: '*' , self p1Name.

	a1 setTraitComposition: (t1 + t2) asTraitComposition
]

{ #category : #tests }
RPackageTraitTest >> testMethodOverridingTraitMethodIsKnowByPackage [
	"Regression test for a bug where adding a method to a class that overrides a method from a trait was not know by the package of the class"

	| a2 |
	a2 := self newClassNamed: #A2DefinedInP1 in: p1.

	a2 setTraitComposition: t1 asTraitComposition.

	self deny: (p1 includesDefinedSelector: #traitMethodDefinedInP1 ofClass: a2).
	self assert: (a2 >> #traitMethodDefinedInP1) isFromTrait.

	a2 compile: 'traitMethodDefinedInP1 "Override in the class"'.

	self assert: (p1 includesDefinedSelector: #traitMethodDefinedInP1 ofClass: a2).
	self deny: (a2 >> #traitMethodDefinedInP1) isFromTrait
]

{ #category : #tests }
RPackageTraitTest >> testPackageOfClassMethodFromTraitExtensionIsExtendingPackage [
	"The package of a method defined in atrait but package in another package than the extended trait is the
	package containing the extension."

	"The package of a method in A1 (which is coming from the trait T1 used by A1) is the package of T1"

	self assert: (a1 >> #traitMethodExtendedFromP1) package equals: p1.
	"The package of a method in A1 (which is coming from the trait T1 used by A1 but extended in package T2) is the package of T2"
	self assert: (a1 >> #traitMethodExtendedFromP3) package equals: p3
]

{ #category : #tests }
RPackageTraitTest >> testPackageOfClassMethodFromTraitIsTraitPackage [
	"test that a class method coming from a trait is packaged in the trait package"

	self assert: (a1 >> #traitMethodDefinedInP1) package equals: p1.
	self assert: (a1 >> #traitMethodDefinedInP2) package equals: p2
]

{ #category : #tests }
RPackageTraitTest >> testPackageOfClassMethodIsClassPackage [
	"The package of a local method (not defined in a trait) is the package of its class"

	self assert: (a1 >> #localMethodDefinedInP1) package equals: p1.
	self assert: (a1 >> #anotherLocalMethodDefinedInP1) package equals: p1.
	self assert: (a1 >> #anotherLocalMethodDefinedInP1) package equals: p1
]

{ #category : #tests }
RPackageTraitTest >> testPackageOfTraitMethodIsTraitPackage [
	"The package of a trait method is the package of its trait."

	self assert: (a1 >> #traitMethodDefinedInP2) package equals: p2.
	self assert: (a1 >> #traitMethodDefinedInP2) package equals: p2.
	self assert: (a1 >> #traitMethodDefinedInP1) package equals: p1
]

{ #category : #tests }
RPackageTraitTest >> testStartingSituation [

	self deny: (p2 includesClass: a1).
	self assert: (p1 includesClass: a1).
	self assert: (p1 includesClass: t1).
	self assert: (p1 definesOrExtendsClass: a1).
	self assert: (p2 includesClass: t2)
]
