"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSpecDeprecated,
	#superclass : #PackageManifest,
	#category : #'Spec-Deprecated-Manifest'
}

{ #category : #deprecation }
ManifestSpecDeprecated class >> isDeprecated [
	^ true
]
