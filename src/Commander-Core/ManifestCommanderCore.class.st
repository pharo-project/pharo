"
I am the manifest of a package of Commander (version 1).
Commander v1 is deprecated: Pharo now uses Commander2 and this package will be removed in a future version of Pharo.
"
Class {
	#name : 'ManifestCommanderCore',
	#superclass : 'PackageManifest',
	#category : 'Commander-Core-Manifest',
	#package : 'Commander-Core',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestCommanderCore class >> isDeprecated [
	"Commander v1 is deprecated and will be removed in Pharo 16."

	^ true
]
