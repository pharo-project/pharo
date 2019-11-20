Class {
	#name : #DAPackageCycleTest,
	#superclass : #TestCase,
	#instVars : [
		'aPDPackageCycle',
		'packageA',
		'packageB',
		'packageC'
	],
	#category : #'Tool-DependencyAnalyser-Tests-Packages'
}

{ #category : #running }
DAPackageCycleTest >> setUp [
	super setUp.
	aPDPackageCycle := DAPackageCycle new.
	packageA := DAPackage on: (RPackageSet named:'A').
	packageB := DAPackage on: (RPackageSet named:'B').
	packageC := DAPackage on: (RPackageSet named:'C').
]

{ #category : #tests }
DAPackageCycleTest >> testIsAtLeastInCycle [
	| aCollectionOfPDPackage |
	aCollectionOfPDPackage  := OrderedCollection new.
	aCollectionOfPDPackage 
		add: packageA;
		add: packageB;
		add: packageC.
	
	aPDPackageCycle
		addPackage: packageA;
		addPackage: packageB.
	
	self assert: (aPDPackageCycle isAtLeastInCycle: aCollectionOfPDPackage).
	aCollectionOfPDPackage remove: packageA; remove: packageB.
	self deny: (aPDPackageCycle isAtLeastInCycle: aCollectionOfPDPackage).
]

{ #category : #tests }
DAPackageCycleTest >> testIsInCycle [
	aPDPackageCycle addPackage: packageA.
	self assert: (aPDPackageCycle isInCycle: packageA).
]

{ #category : #running }
DAPackageCycleTest >> testRemoveOutgoingDependencies [
	| firstPackage secondPackage outgoingPackage otherOutgoingPackage |
	firstPackage := DAPackage on: (RPackageSet named: 'A').
	secondPackage := DAPackage on: (RPackageSet named: 'B').
	outgoingPackage := DAPackage on: (RPackageSet named: 'C').
	otherOutgoingPackage := DAPackage on: (RPackageSet named: 'D').
	firstPackage add: (DACompositeDependency from: firstPackage to: secondPackage).
	firstPackage add: (DACompositeDependency from: firstPackage to: outgoingPackage).
	firstPackage add: (DACompositeDependency from: firstPackage to: otherOutgoingPackage).
	secondPackage add: (DACompositeDependency from: secondPackage to: firstPackage).
	secondPackage add: (DACompositeDependency from: secondPackage to: outgoingPackage).
	aPDPackageCycle addPackage: firstPackage; addPackage: secondPackage.
	aPDPackageCycle removeOutgoingDependencies.
	"at this point, package A and B does not have dependency with C and D, because C is not in the cycle"
	"self halt."
	self deny: (aPDPackageCycle hasOutgoingDependencies: firstPackage).
	self deny: (aPDPackageCycle hasOutgoingDependencies: secondPackage).
]

{ #category : #tests }
DAPackageCycleTest >> testReversedCycle [
	aPDPackageCycle
		addPackage: packageA;
		addPackage: packageB;
		addPackage: packageC;
		yourself.
	aPDPackageCycle reversedCycle.
	self assert: (aPDPackageCycle packageAt: 1) equals: packageC.
	self assert: (aPDPackageCycle packageAt: 2) equals: packageB.
	self assert: (aPDPackageCycle packageAt: 3) equals: packageA
]

{ #category : #tests }
DAPackageCycleTest >> testSizeOfCycles [
	self assert: aPDPackageCycle size equals: 0.
	aPDPackageCycle addPackage: (DAPackage on: (RPackageSet named: 'Kernel')).
	aPDPackageCycle addPackage: (DAPackage on: (RPackageSet named: 'Collections-Abstract')).
	self assert: aPDPackageCycle size equals: 2
]
