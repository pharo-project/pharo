"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestEmbeddedFreeType',
	#superclass : 'PackageManifest',
	#category : 'EmbeddedFreeType-Manifest',
	#package : 'EmbeddedFreeType',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestEmbeddedFreeType class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-Character-Encoding-Core')
]
