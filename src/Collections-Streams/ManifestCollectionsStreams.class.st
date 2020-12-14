"
All Stream hierarchy: read/write stream
"
Class {
	#name : #ManifestCollectionsStreams,
	#superclass : #PackageManifest,
	#category : #'Collections-Streams-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsStreams class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Support' #'Collections-Sequenceable' #Kernel #'Collections-Native')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsStreams class >> manuallyResolvedDependencies [
	^ #(#UIManager #'Collections-Abstract')
]

{ #category : #'meta-data' }
ManifestCollectionsStreams class >> packageName [
	^ #'Collections-Streams'
]
