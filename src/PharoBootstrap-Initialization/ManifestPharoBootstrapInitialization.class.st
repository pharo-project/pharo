"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestPharoBootstrapInitialization,
	#superclass : #PackageManifest,
	#category : #PharoBootstrap-Initialization
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
