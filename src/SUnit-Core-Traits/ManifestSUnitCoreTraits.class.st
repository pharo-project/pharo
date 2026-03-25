"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSUnitCoreTraits',
	#superclass : 'PackageManifest',
	#category : 'SUnit-Core-Traits-Manifest',
	#package : 'SUnit-Core-Traits',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSUnitCoreTraits class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Time')
]
