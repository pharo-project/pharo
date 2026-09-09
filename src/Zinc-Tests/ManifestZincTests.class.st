"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZincTests',
	#superclass : 'PackageManifest',
	#category : 'Zinc-Tests-Manifest',
	#package : 'Zinc-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestZincTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Benchmark')
]
