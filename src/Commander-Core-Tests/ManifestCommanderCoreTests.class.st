"
I am the manifest of a package of Commander (version 1).
Commander v1 is deprecated: Pharo now uses Commander2 and this package will be removed in a future version of Pharo.
"
Class {
	#name : 'ManifestCommanderCoreTests',
	#superclass : 'PackageManifest',
	#category : 'Commander-Core-Tests-Manifest',
	#package : 'Commander-Core-Tests',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestCommanderCoreTests class >> isDeprecated [
	"Commander v1 is deprecated and will be removed in Pharo 16."

	^ true
]
