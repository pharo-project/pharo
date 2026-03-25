"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZincCharacterEncodingTests',
	#superclass : 'PackageManifest',
	#category : 'Zinc-Character-Encoding-Tests-Manifest',
	#package : 'Zinc-Character-Encoding-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestZincCharacterEncodingTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Benchmark' #'Random-Core' #'FileSystem-Core' #'Zinc-HTTP')
]
