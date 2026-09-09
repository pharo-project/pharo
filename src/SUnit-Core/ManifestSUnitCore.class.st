"
Core package for the SUnit unit testing framework
"
Class {
	#name : 'ManifestSUnitCore',
	#superclass : 'PackageManifest',
	#category : 'SUnit-Core-Manifest',
	#package : 'SUnit-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSUnitCore class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Transcript-NonInteractive')
]
