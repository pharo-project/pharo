Class {
	#name : 'PackageDeprecationTest',
	#superclass : 'TestCase',
	#instVars : [
		'class'
	],
	#category : 'Manifest-Tests',
	#package : 'Manifest-Tests'
}

{ #category : 'tests' }
PackageDeprecationTest >> packageNameForTests [
	^ #'Generated-Compiled-Method-Test-Package'
]

{ #category : 'running' }
PackageDeprecationTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.
	class ifNotNil: [ class removeFromSystem ].
	super tearDown
]

{ #category : 'tests' }
PackageDeprecationTest >> testMethodInDeprecatedPackageIsDeprecated [

	self class compile: 'method ^1' classified: '*' , self packageNameForTests.

	self deny: self packageNameForTests asPackage isDeprecated.
	self deny: (self class >> #method) isDeprecated.

	class := self packageNameForTests asPackage packageManifest.
	class class compile: 'isDeprecated ^ true'.

	self assert: self packageNameForTests asPackage isDeprecated.
	self assert: (self class >> #method) isDeprecated
]
