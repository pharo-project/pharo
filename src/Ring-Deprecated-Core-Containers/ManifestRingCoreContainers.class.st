Class {
	#name : #ManifestRingCoreContainers,
	#superclass : #PackageManifest,
	#category : #'Ring-Deprecated-Core-Containers'
}

{ #category : #deprecation }
ManifestRingCoreContainers class >> isDeprecated [
	^true
]

{ #category : #'meta-data - dependency analyser' }
ManifestRingCoreContainers class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
