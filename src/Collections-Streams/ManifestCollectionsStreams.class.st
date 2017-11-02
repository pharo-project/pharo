"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsStreams,
	#superclass : #PackageManifest,
	#category : 'Collections-Streams'
}

{ #category : #meta-data }
ManifestCollectionsStreams class >> dependencies [
	^ #(#'Collections-Strings' #'Multilingual-TextConversion' #'Collections-Support' #'Collections-Sequenceable' #Kernel #'Collections-Native')
]

{ #category : #meta-data }
ManifestCollectionsStreams class >> description [
	^ 'All Stream hierarchy: read/write stream'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsStreams class >> ignoredDependencies [
	^ #(#'Transcript-Tool')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsStreams class >> manuallyResolvedDependencies [
	^ #(#UIManager #'Transcript-Tool' #'Transcript-Core' #'Collections-Abstract' #Jobs)
]

{ #category : #meta-data }
ManifestCollectionsStreams class >> packageName [
	^ #'Collections-Streams'
]
