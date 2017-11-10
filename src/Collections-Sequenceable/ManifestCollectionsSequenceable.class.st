"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsSequenceable,
	#superclass : #PackageManifest,
	#category : #Collections-Sequenceable
}

{ #category : #meta-data }
ManifestCollectionsSequenceable class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Streams' #'Collections-Unordered' #Kernel #'Collections-Abstract')
]

{ #category : #meta-data }
ManifestCollectionsSequenceable class >> description [
	^ 'Sequenceable collections: arrays, intervals, ordered collections and dictionaries, etc.'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsSequenceable class >> manuallyResolvedDependencies [
	^ #(#'Collections-Support' #'System-Support')
]

{ #category : #meta-data }
ManifestCollectionsSequenceable class >> packageName [
	^ #'Collections-Sequenceable'
]
