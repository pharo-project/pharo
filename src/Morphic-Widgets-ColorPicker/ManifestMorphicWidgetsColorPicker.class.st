"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestMorphicWidgetsColorPicker',
	#superclass : 'PackageManifest',
	#category : 'Morphic-Widgets-ColorPicker-Manifest',
	#package : 'Morphic-Widgets-ColorPicker',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMorphicWidgetsColorPicker class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Graphics-Files' #'System-Object Events' #'System-Localization')
]
