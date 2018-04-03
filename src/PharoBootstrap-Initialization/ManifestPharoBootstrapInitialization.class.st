Class {
	#name : #ManifestPharoBootstrapInitialization,
	#superclass : #PackageManifest,
	#category : #'PharoBootstrap-Initialization'
}

{ #category : #'as yet unclassified' }
ManifestPharoBootstrapInitialization class >> ignoredDependencies [
	^ #(#'SUnit-Core')
]

{ #category : #'meta-data - dependency analyser' }
ManifestPharoBootstrapInitialization class >> manuallyResolvedDependencies [
	^ #(#'System-Finalization' #'SUnit-Core')
]

{ #category : #'meta-data - dependency analyser' }
ManifestPharoBootstrapInitialization class >> packageName [
	^ #'PharoBootstrap-Initialization'
]
