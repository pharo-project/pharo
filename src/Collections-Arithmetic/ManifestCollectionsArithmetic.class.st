"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsArithmetic,
	#superclass : #PackageManifest,
	#category : #'Collections-Arithmetic'
}

{ #category : #'meta-data' }
ManifestCollectionsArithmetic class >> dependencies [
	^ #(#'Collections-Native' #'Collections-Unordered' #'Collections-Sequenceable' #Kernel #'Collections-Abstract')
]

{ #category : #'meta-data' }
ManifestCollectionsArithmetic class >> description [
	^ 'This package only makes extensions to existing collection classes by adding the arithmetic protocols. 
Such protocols allow one to perform in particular vector-operations on collection'
]

{ #category : #'meta-data' }
ManifestCollectionsArithmetic class >> packageName [
	^ #'Collections-Arithmetic'
]
