"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestNetworkProtocols',
	#superclass : 'PackageManifest',
	#category : 'Network-Protocols-Manifest',
	#package : 'Network-Protocols',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestNetworkProtocols class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-Character-Encoding-Core')
]
