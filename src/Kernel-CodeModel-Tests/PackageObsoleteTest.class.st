"
SUnit tests for Package
"
Class {
	#name : 'PackageObsoleteTest',
	#superclass : 'PackageTestCase',
	#category : 'Kernel-CodeModel-Tests-Packages',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Packages'
}

{ #category : 'tests' }
PackageObsoleteTest >> testMethodPackageFromObsoleteClass [

	| pack method foo |
	pack := self ensurePackage: 'P1'.
	foo := self newClassNamed: #FooForTest in: pack.
	foo compile: 'bar ^42'.
	method := foo >> #bar.

	[
	foo obsolete.
	self assert: method package equals: foo package ] ensure: [
		foo ifNotNil: [
			foo setName: foo originalName.
			foo removeFromSystem ].
		testingEnvironment removeKey: #FooForTest ifAbsent: [  ] ]
]

{ #category : 'tests' }
PackageObsoleteTest >> testMethodPackageOfRemovedClass [

	| pack method foo |
	pack := self ensurePackage: 'P1'.
	foo := self newClassNamed: #FooForTest2 in: pack.
	foo compile: 'bar ^42'.
	method := foo >> #bar.
	foo removeFromSystem.
	self deny: (pack includesClassNamed: #FooForTest2)
]
