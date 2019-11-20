Class {
	#name : #DAPackageDependencyWrapperTest,
	#superclass : #TestCase,
	#instVars : [
		'packageA',
		'aPDPackageDependencyWrapper'
	],
	#category : #'Tool-DependencyAnalyser-Tests-Packages'
}

{ #category : #running }
DAPackageDependencyWrapperTest >> setUp [
	super setUp.
	packageA := DAPackage on: (RPackageSet named:'A').
]

{ #category : #tests }
DAPackageDependencyWrapperTest >> testInitializeFrom [
	aPDPackageDependencyWrapper := DAPackageDependencyWrapper newWith: packageA.
	self assert: aPDPackageDependencyWrapper daPackage equals: packageA
]
