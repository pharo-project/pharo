Class {
	#name : #DAPackageDependencyWrapperTest,
	#superclass : #TestCase,
	#instVars : [
		'packageA',
		'aPDPackageDependencyWrapper'
	],
	#category : #Tool-DependencyAnalyser-Test
}

{ #category : #running }
DAPackageDependencyWrapperTest >> setUp [
	packageA := DAPackage on: (RPackageSet named:'A').
]

{ #category : #tests }
DAPackageDependencyWrapperTest >> testInitializeFrom [
	aPDPackageDependencyWrapper := DAPackageDependencyWrapper newWith: packageA.
	self assert: (aPDPackageDependencyWrapper daPackage = packageA).
]
