"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestThreadedFFI',
	#superclass : 'PackageManifest',
	#category : 'ThreadedFFI-Manifest',
	#package : 'ThreadedFFI',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestThreadedFFI class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Transcript-NonInteractive')
]
