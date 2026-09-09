"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestUnifiedFFI',
	#superclass : 'PackageManifest',
	#category : 'UnifiedFFI-Manifest',
	#package : 'UnifiedFFI',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestUnifiedFFI class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-Character-Encoding-Core')
]
