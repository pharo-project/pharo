Class {
	#name : 'RePackageManifestShouldBePackagedInManifestTagRule',
	#superclass : 'ReAbstractRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'testing - interest' }
RePackageManifestShouldBePackagedInManifestTagRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
RePackageManifestShouldBePackagedInManifestTagRule class >> group [

	^ 'Coding Idiom Violation'
]

{ #category : 'running' }
RePackageManifestShouldBePackagedInManifestTagRule >> basicCheck: aClass [

	^ (aClass inheritsFrom: PackageManifest) and: [ ((aClass package classesTaggedWith: 'Manifest') includes: aClass) not ]
]

{ #category : 'accessing' }
RePackageManifestShouldBePackagedInManifestTagRule >> name [
	^ 'A package manifest should be tagged using ''Manifest'' class tag'
]

{ #category : 'accessing' }
RePackageManifestShouldBePackagedInManifestTagRule >> rationale [
	^ 'Check if the package manifest is tagged in class category ''Manifest'' to align with other packages and fulfil common expectations.'
]
