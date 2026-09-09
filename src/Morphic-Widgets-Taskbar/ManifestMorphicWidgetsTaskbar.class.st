"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestMorphicWidgetsTaskbar',
	#superclass : 'PackageManifest',
	#category : 'Morphic-Widgets-Taskbar-Manifest',
	#package : 'Morphic-Widgets-Taskbar',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMorphicWidgetsTaskbar class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Localization')
]
