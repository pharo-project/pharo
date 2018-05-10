"
Arrays of native types: byte, word, float and integer
"
Class {
	#name : #ManifestCollectionsNative,
	#superclass : #PackageManifest,
	#category : #'Collections-Native-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsNative class >> dependencies [
	^ #(#'Collections-Abstract' #Kernel #'Collections-Strings')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsNative class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]

{ #category : #'meta-data' }
ManifestCollectionsNative class >> packageName [
	^ #'Collections-Native'
]
