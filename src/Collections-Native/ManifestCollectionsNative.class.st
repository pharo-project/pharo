"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsNative,
	#superclass : #PackageManifest,
	#category : #Collections-Native
}

{ #category : #meta-data }
ManifestCollectionsNative class >> dependencies [
	^ #(#'Collections-Abstract' #Kernel #'Collections-Strings')
]

{ #category : #meta-data }
ManifestCollectionsNative class >> description [
	^ 'Arrays of native types: byte, word, float and integer'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsNative class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]

{ #category : #meta-data }
ManifestCollectionsNative class >> packageName [
	^ #'Collections-Native'
]
