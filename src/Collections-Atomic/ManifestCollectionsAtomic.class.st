Class {
	#name : #ManifestCollectionsAtomic,
	#superclass : #PackageManifest,
	#category : #'Collections-Atomic'
}

{ #category : #'meta-data' }
ManifestCollectionsAtomic class >> dependencies [
	^ #(#Kernel)
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsAtomic class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]

{ #category : #'meta-data' }
ManifestCollectionsAtomic class >> packageName [
	^ #'Collections-Atomic'
]
