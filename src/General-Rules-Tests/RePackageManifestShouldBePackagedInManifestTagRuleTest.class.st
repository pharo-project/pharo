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

	| manifest |
	manifest := classfactory make: [ :builder |
		            builder
			            superclass: PackageManifest;
			            tag: 'SomeWhere' ].
	self assert: (self myCritiquesOnClass: manifest) size equals: 1
]

{ #category : 'tests' }
RePackageManifestShouldBePackagedInManifestTagRuleTest >> testRuleNotViolated [

	| manifest |
	manifest := classfactory make: [ :builder |
		            builder
			            superclass: PackageManifest;
			            tag: 'Manifest' ].
	self assertEmpty: (self myCritiquesOnClass: manifest)
]
