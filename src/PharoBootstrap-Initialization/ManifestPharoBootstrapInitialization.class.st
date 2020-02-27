"
Initializations done during image bootstrap process
"
Class {
	#name : #ManifestPharoBootstrapInitialization,
	#superclass : #PackageManifest,
	#category : #'PharoBootstrap-Initialization-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestPharoBootstrapInitialization class >> ignoredDependencies [
	^ #(#'SUnit-Core')
]

{ #category : #'meta-data - dependency analyser' }
ManifestPharoBootstrapInitialization class >> manuallyResolvedDependencies [
	^ #(#'System-Finalization')
]

{ #category : #'meta-data - dependency analyser' }
ManifestPharoBootstrapInitialization class >> packageName [
	^ #'PharoBootstrap-Initialization'
]
