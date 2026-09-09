"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemOSEnvironments',
	#superclass : 'PackageManifest',
	#category : 'System-OSEnvironments-Manifest',
	#package : 'System-OSEnvironments',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemOSEnvironments class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-Character-Encoding-Core')
]
