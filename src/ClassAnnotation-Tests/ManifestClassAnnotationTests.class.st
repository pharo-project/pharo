"
Deprecated. The tests of ClassAnnotation, which will be removed in Pharo 16 (see ManifestClassAnnotation).
"
Class {
	#name : 'ManifestClassAnnotationTests',
	#superclass : 'PackageManifest',
	#category : 'ClassAnnotation-Tests-Manifest',
	#package : 'ClassAnnotation-Tests',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestClassAnnotationTests class >> isDeprecated [
	"ClassAnnotation is only used by the deprecated Commander 1 and Commander-Calypso-Compatibility, and will be removed with them in Pharo 16."

	^ true
]
