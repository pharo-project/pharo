"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestMorphicWidgetsFastTable',
	#superclass : 'PackageManifest',
	#category : 'Morphic-Widgets-FastTable-Manifest',
	#package : 'Morphic-Widgets-FastTable',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMorphicWidgetsFastTable class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Object Events' #'Transcript-NonInteractive')
]
