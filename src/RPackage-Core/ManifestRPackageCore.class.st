"
Manifest for RPackage-Core
"
Class {
	#name : 'ManifestRPackageCore',
	#superclass : 'PackageManifest',
	#category : 'RPackage-Core-Manifest',
	#package : 'RPackage-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestRPackageCore class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Collections-Streams' #'Collections-Strings' #'Collections-Abstract')
]
