"
I am the manifest of a package of Commander (version 1).
Commander v1 is deprecated: Pharo now uses Commander2 and this package will be removed in a future version of Pharo.
"
Class {
	#name : 'ManifestCommanderActivatorsShortcut',
	#superclass : 'PackageManifest',
	#category : 'Commander-Activators-Shortcut-Manifest',
	#package : 'Commander-Activators-Shortcut',
	#tag : 'Manifest'
}

{ #category : 'deprecation' }
ManifestCommanderActivatorsShortcut class >> isDeprecated [
	"Commander v1 is deprecated and will be removed in Pharo 16."

	^ true
]

{ #category : 'meta-data - dependency analyser' }
ManifestCommanderActivatorsShortcut class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Object Events')
]
