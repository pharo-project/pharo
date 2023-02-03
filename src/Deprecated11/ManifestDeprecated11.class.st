"
Deprecated package of Pharo 11
"
Class {
	#name : #ManifestDeprecated11,
	#superclass : #PackageManifest,
	#category : #'Deprecated11-Manifest'
}

{ #category : #testing }
ManifestDeprecated11 class >> isDeprecated [
	^ true
]

{ #category : #testing }
ManifestDeprecated11 >> isDeprecated [
	^ self class isDeprecated
]
