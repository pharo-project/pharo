"
rule checking that the package manifest has the tag ''Manifest''
"
Class {
	#name : 'RePackageManifestShouldBePackagedInManifestTagRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'testing - interest' }
RePackageManifestShouldBePackagedInManifestTagRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
RePackageManifestShouldBePackagedInManifestTagRule class >> group [

	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
RePackageManifestShouldBePackagedInManifestTagRule class >> rationale [
	^ 'Check if the package manifest is tagged in class category ''Manifest'' to align with other packages and fulfil common expectations.'
]

{ #category : 'accessing' }
RePackageManifestShouldBePackagedInManifestTagRule class >> ruleName [
	^ 'A package manifest should be tagged using ''Manifest'' class tag'
]

{ #category : 'running' }
RePackageManifestShouldBePackagedInManifestTagRule >> basicCheck: aClass [

	^ (aClass inheritsFrom: PackageManifest) and: [ ((aClass package classesTaggedWith: 'Manifest') includes: aClass) not ]
]
