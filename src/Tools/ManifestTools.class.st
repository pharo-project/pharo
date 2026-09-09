"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestTools',
	#superclass : 'PackageManifest',
	#category : 'Tools-Manifest',
	#package : 'Tools',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestTools class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Transcript-NonInteractive')
]
