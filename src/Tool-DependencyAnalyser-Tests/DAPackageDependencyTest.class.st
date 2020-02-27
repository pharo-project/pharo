Class {
	#name : #DAPackageDependencyTest,
	#superclass : #TestCase,
	#instVars : [
		'aPackageDependency',
		'anInternalPackageDependency',
		'packageA',
		'packageB'
	],
	#category : #'Tool-DependencyAnalyser-Tests-Packages'
}

{ #category : #running }
DAPackageDependencyTest >> inheritanceDependency: aClass [
	^ (DAInheritanceDependency from: packageA to: packageB)
		theClass: aClass;
		superclass: nil;
		yourself
]

{ #category : #running }
DAPackageDependencyTest >> referenceDependency: aClass [
	^ (DAReferenceDependency from: packageA to: packageB)
		theClass: aClass;
		selector: nil;
		reference: nil;
		method: nil;
		yourself
]

{ #category : #running }
DAPackageDependencyTest >> setUp [
	| source target |
	super setUp.
	source := DAPackage on: (RPackageSet named:'Kernel').
	target := DAPackage on: (RPackageSet named:'Collections-Abstract').
	aPackageDependency := DAPackageDependency from:source to:target.
	anInternalPackageDependency := DAPackageDependency from:source to:source.
	packageA := DAPackage on: (RPackageSet named:'A').
	packageB := DAPackage on: (RPackageSet named:'B').
]

{ #category : #tests }
DAPackageDependencyTest >> testContainsOnlyExtensionDependency [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency
		from: (packageA)
		to: (packageB).
	aCompositeDependency
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB));
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB)).
	self deny: (aCompositeDependency containsOnlyExtensionDependency).
	aCompositeDependency dependencies removeAll.
		aCompositeDependency
		add:
			(DAExtensionDependency from: (packageA) to: (packageB));
		add:
			(DAExtensionDependency from: (packageA) to: (packageB)).
	self assert: (aCompositeDependency containsOnlyExtensionDependency).
		
]

{ #category : #tests }
DAPackageDependencyTest >> testDependenciesFrom [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency
		add: (self referenceDependency: String);
		add: (self referenceDependency: Object);
		add: (self referenceDependency: String);
		add: (self inheritanceDependency: Stack).
	self assert: (aCompositeDependency referenceDependenciesFrom: String) size equals: 2
]

{ #category : #tests }
DAPackageDependencyTest >> testDependenciesSize [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency
		add: (DAInheritanceDependency from: packageA to: packageB);
		add: (DAInheritanceDependency from: packageA to: packageB).
	self assert: aCompositeDependency extensionDependenciesSize equals: 0.
	aCompositeDependency add: (DAExtensionDependency from: packageA to: packageB).
	self assert: aCompositeDependency extensionDependenciesSize equals: 1
]

{ #category : #tests }
DAPackageDependencyTest >> testDependenciesWithoutExtensionDependency [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency
		add: (DAInheritanceDependency from: packageA to: packageB);
		add: (DAInheritanceDependency from: packageA to: packageB);
		add: (DAExtensionDependency from: packageA to: packageB).
	self assert: aCompositeDependency dependenciesWithoutExtensionDependency size equals: 2
]

{ #category : #tests }
DAPackageDependencyTest >> testEquals [
	self assert: aPackageDependency equals: aPackageDependency
]

{ #category : #tests }
DAPackageDependencyTest >> testExtensionDependencies [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency
		from: (packageA)
		to: (packageB).
	aCompositeDependency
		add:
			(DAExtensionDependency from: (packageA) to: (packageB));
		add:
			(DAInheritanceDependency from: (packageA) to: (packageB)).
	self assert: (aCompositeDependency extensionDependencies allSatisfy: [ :each | each isExtensionDependency ]).
]

{ #category : #tests }
DAPackageDependencyTest >> testIsExternal [
	self assert: (aPackageDependency isExternal)
]

{ #category : #tests }
DAPackageDependencyTest >> testIsInternal [
	self assert: (anInternalPackageDependency isInternal)
]

{ #category : #tests }
DAPackageDependencyTest >> testReferenceDependencies [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency
		add: (self referenceDependency: String);
		add: (self referenceDependency: Object);
		add: (DAInheritanceDependency from: packageA to: packageB).

	self assert: aCompositeDependency referenceDependencies size equals: 2
]

{ #category : #tests }
DAPackageDependencyTest >> testReferenceDependenciesFrom [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency
		add: (self referenceDependency: String);
		add: (self referenceDependency: Object);
		add: (self referenceDependency: String).

	self assert: (aCompositeDependency referenceDependenciesFrom: String) size equals: 2
]
