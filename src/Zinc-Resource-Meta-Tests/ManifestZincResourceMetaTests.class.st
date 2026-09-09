"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZincResourceMetaTests',
	#superclass : 'PackageManifest',
	#category : 'Zinc-Resource-Meta-Tests-Manifest',
	#package : 'Zinc-Resource-Meta-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestZincResourceMetaTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-HTTP')
]
