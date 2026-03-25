"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestThreadedFFITests',
	#superclass : 'PackageManifest',
	#category : 'ThreadedFFI-Tests-Manifest',
	#package : 'ThreadedFFI-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestThreadedFFITests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-Character-Encoding-Core')
]
