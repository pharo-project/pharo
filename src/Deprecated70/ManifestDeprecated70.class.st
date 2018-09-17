"
This package includes all the methods and classes to be deprecated in Pharo 7.0. This code will be removed in Pharo 8.0
"
Class {
	#name : #ManifestDeprecated70,
	#superclass : #PackageManifest,
	#category : #'Deprecated70-Manifest'
}

{ #category : #testing }
ManifestDeprecated70 class >> isDeprecated [
	"This package is also deprecated as with the next development cycle it will be removed"
	
	^true
]
