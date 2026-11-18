"
System support for object events (change/update mechanism)
"
Class {
	#name : 'ManifestSystemObjectEvents',
	#superclass : 'PackageManifest',
	#category : 'System-Object Events-Manifest',
	#package : 'System-Object Events',
	#tag : 'Manifest'
}

{ #category : 'testing' }
ManifestSystemObjectEvents class >> deprecatedIn [

	^ #Pharo15
]

{ #category : 'testing' }
ManifestSystemObjectEvents class >> isDeprecated [
	"Symbol-keyed object events. Use announcements instead"

	^ true
]

{ #category : 'meta-data - dependency analyser' }
ManifestSystemObjectEvents class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
