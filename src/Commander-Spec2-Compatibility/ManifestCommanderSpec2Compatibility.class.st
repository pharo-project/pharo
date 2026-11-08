"
I am the manifest of a package of Commander (version 1).
Commander v1 is deprecated: Pharo now uses Commander2 and this package will be removed in a future version of Pharo.
"
Class {
	#name : 'ManifestCommanderSpec2Compatibility',
	#superclass : 'PackageManifest',
	#category : 'Commander-Spec2-Compatibility-Manifest',
	#package : 'Commander-Spec2-Compatibility',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestCommanderSpec2Compatibility class >> isDeprecated [
	"Commander v1 is deprecated and will be removed in Pharo 16."

	^ true
]
