"
All Stream hierarchy: read/write stream
"
Class {
	#name : #ManifestCollectionsStreams,
	#superclass : #PackageManifest,
	#category : #'Collections-Streams'
}

{ #category : #'meta-data' }
ManifestCollectionsStreams class >> dependencies [
	^ #(#'Collections-Strings' #'Multilingual-TextConversion' #'Collections-Support' #'Collections-Sequenceable' #Kernel #'Collections-Native')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsStreams class >> manuallyResolvedDependencies [
	^ #(#UIManager #'Collections-Abstract' #Jobs)
]

{ #category : #'meta-data' }
ManifestCollectionsStreams class >> packageName [
	^ #'Collections-Streams'
]
