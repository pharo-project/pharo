Class {
	#name : #RePackageManifestShouldBePackagedInManifestTag,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing - interest' }
RePackageManifestShouldBePackagedInManifestTag class >> checksClass [

	^ true
]

{ #category : #running }
RePackageManifestShouldBePackagedInManifestTag >> basicCheck: aClass [

	^ (aClass inheritsFrom: PackageManifest) and: [ 
		((aClass package classesForClassTag: 'Manifest') includes: aClass) not ]
]

{ #category : #accessing }
RePackageManifestShouldBePackagedInManifestTag >> group [

	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RePackageManifestShouldBePackagedInManifestTag >> name [
	^ 'A package manifest should be tagged using ''Manifest'' class tag'
]

{ #category : #accessing }
RePackageManifestShouldBePackagedInManifestTag >> rationale [
	^ 'Check if the package manifest is tagged in class category ''Manifest'' to align with other packages and fulfil common expectations.'
]

{ #category : #accessing }
RePackageManifestShouldBePackagedInManifestTag >> severity [

	^ #warning 
]
