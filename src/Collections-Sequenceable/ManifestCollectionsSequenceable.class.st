"
Sequenceable collections: arrays, intervals, ordered collections and dictionaries, etc.
"
Class {
	#name : #ManifestCollectionsSequenceable,
	#superclass : #PackageManifest,
	#category : #'Collections-Sequenceable-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsSequenceable class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Streams' #'Collections-Unordered' #Kernel #'Collections-Abstract')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsSequenceable class >> manuallyResolvedDependencies [
	^ #(#'Collections-Support' #'System-Support')
]

{ #category : #'meta-data' }
ManifestCollectionsSequenceable class >> packageName [
	^ #'Collections-Sequenceable'
]
