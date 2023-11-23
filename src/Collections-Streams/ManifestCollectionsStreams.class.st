"
All Stream hierarchy: read/write stream
"
Class {
	#name : 'ManifestCollectionsStreams',
	#superclass : 'PackageManifest',
	#category : 'Collections-Streams-Manifest',
	#package : 'Collections-Streams',
	#tag : 'Manifest'
}

{ #category : 'meta-data' }
ManifestCollectionsStreams class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Support' #'Collections-Sequenceable' #Kernel #'Collections-Native')
]

{ #category : 'meta-data' }
ManifestCollectionsStreams class >> packageName [
	^ #'Collections-Streams'
]
