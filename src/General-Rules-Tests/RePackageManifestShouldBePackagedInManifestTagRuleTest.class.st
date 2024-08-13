Class {
	#name : 'RePackageManifestShouldBePackagedInManifestTagRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classfactory'
	],
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'running' }
RePackageManifestShouldBePackagedInManifestTagRuleTest >> setUp [

	super setUp.
	classfactory := ClassFactoryForTestCase new.
]

{ #category : 'running' }
RePackageManifestShouldBePackagedInManifestTagRuleTest >> tearDown [ 

	classfactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
RePackageManifestShouldBePackagedInManifestTagRuleTest >> testRule [

	| critiques manifest | 
	manifest := classfactory make: [ :builder | builder superclass: PackageManifest;
		tag: 'SomeWhere' ].
	
	critiques := self myCritiquesOnClass: manifest.
	self assert: critiques size equals: 1
	
	
]

{ #category : 'tests' }
RePackageManifestShouldBePackagedInManifestTagRuleTest >> testRuleNotViolated [

	| critiques manifest | 
	manifest := classfactory make: [ :builder | builder superclass: PackageManifest;
		tag: 'Manifest' ].
	
	critiques := self myCritiquesOnClass: manifest.
	self assertEmpty: critiques
	
	
]
