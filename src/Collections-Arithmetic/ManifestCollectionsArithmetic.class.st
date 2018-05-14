"
This package only makes extensions to existing collection classes by adding the arithmetic protocols. 
Such protocols allow one to perform in particular vector-operations on collection
"
Class {
	#name : #ManifestCollectionsArithmetic,
	#superclass : #PackageManifest,
	#category : #'Collections-Arithmetic-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsArithmetic class >> dependencies [
	^ #(#'Collections-Native' #'Collections-Unordered' #'Collections-Sequenceable' #Kernel #'Collections-Abstract')
]

{ #category : #'meta-data' }
ManifestCollectionsArithmetic class >> packageName [
	^ #'Collections-Arithmetic'
]
