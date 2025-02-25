Class {
	#name : 'DAPackageDependencyWrapperTest',
	#superclass : 'TestCase',
	#instVars : [
		'packageA',
		'aPDPackageDependencyWrapper'
	],
	#category : 'Tool-DependencyAnalyser-Tests-Packages',
	#package : 'Tool-DependencyAnalyser-Tests',
	#tag : 'Packages'
}

{ #category : 'running' }
DAPackageDependencyWrapperTest >> setUp [

	super setUp.
	packageA := DAPackage onPackageNamed: 'A'
]

{ #category : 'tests' }
DAPackageDependencyWrapperTest >> testInitializeFrom [
	aPDPackageDependencyWrapper := DAPackageDependencyWrapper newWith: packageA.
	self assert: aPDPackageDependencyWrapper daPackage equals: packageA
]
