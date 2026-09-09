Class {
	#name : 'ManifestReflectivityTests',
	#superclass : 'PackageManifest',
	#category : 'Reflectivity-Tests-Manifest',
	#package : 'Reflectivity-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestReflectivityTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Transcript-NonInteractive')
]
