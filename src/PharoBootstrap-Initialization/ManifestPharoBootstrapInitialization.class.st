"
Initializations done during image bootstrap process
"
Class {
	#name : 'ManifestPharoBootstrapInitialization',
	#superclass : 'PackageManifest',
	#category : 'PharoBootstrap-Initialization-Manifest',
	#package : 'PharoBootstrap-Initialization',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestPharoBootstrapInitialization class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Collections-Abstract' #'Collections-Sequenceable')
]

{ #category : 'meta-data - dependency analyser' }
ManifestPharoBootstrapInitialization class >> packageName [
	^ #'PharoBootstrap-Initialization'
]
