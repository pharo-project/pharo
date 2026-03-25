"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestCommanderActivatorsShortcut',
	#superclass : 'PackageManifest',
	#category : 'Commander-Activators-Shortcut-Manifest',
	#package : 'Commander-Activators-Shortcut',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestCommanderActivatorsShortcut class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Object Events')
]
