"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZincCharacterEncodingCore',
	#superclass : 'PackageManifest',
	#category : 'Zinc-Character-Encoding-Core-Manifest',
	#package : 'Zinc-Character-Encoding-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestZincCharacterEncodingCore class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-Resource-Meta-Core')
]
