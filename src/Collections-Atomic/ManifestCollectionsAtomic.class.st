"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsAtomic,
	#superclass : #PackageManifest,
	#category : #Collections-Atomic
}

{ #category : #meta-data }
ManifestCollectionsAtomic class >> dependencies [
	^ #(#Kernel)
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsAtomic class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]

{ #category : #meta-data }
ManifestCollectionsAtomic class >> packageName [
	^ #'Collections-Atomic'
]
