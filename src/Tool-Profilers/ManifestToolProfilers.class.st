"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestToolProfilers',
	#superclass : 'PackageManifest',
	#category : 'Tool-Profilers-Manifest',
	#package : 'Tool-Profilers',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestToolProfilers class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Localization' #'Transcript-NonInteractive')
]
