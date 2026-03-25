"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestMorphicWidgetsTabs',
	#superclass : 'PackageManifest',
	#category : 'Morphic-Widgets-Tabs-Manifest',
	#package : 'Morphic-Widgets-Tabs',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMorphicWidgetsTabs class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Graphics-Files' #'System-Object Events' #'Kernel-CodeModel' #'System-Time')
]
