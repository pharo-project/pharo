"
Package manifest for package metadata
"
Class {
	#name : #ManifestCollectionsAbstract,
	#superclass : #PackageManifest,
	#category : #'Collections-Abstract-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsAbstract class >> dependencies [
	^ #(#'Regex-Core' #'Collections-Strings' #'Collections-Streams' #'Collections-Support' #'Collections-Unordered' #Kernel #'Collections-Sequenceable' #'Collections-Native')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsAbstract class >> manuallyResolvedDependencies [
	^ #(#'Collections-Arithmetic')
]

{ #category : #'meta-data' }
ManifestCollectionsAbstract class >> packageName [
	^ #'Collections-Abstract'
]
