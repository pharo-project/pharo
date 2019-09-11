"
I contain collections that do not care about the order of the elements they contain.
Main classes: Bag, Dictionary, Matrix, Set
"
Class {
	#name : #ManifestCollectionsUnordered,
	#superclass : #PackageManifest,
	#category : #'Collections-Unordered-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsUnordered class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Support' #Kernel #'Collections-Sequenceable' #'Collections-Abstract')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsUnordered class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'Math-Operations-Extensions' #'System-Support')
]

{ #category : #'meta-data' }
ManifestCollectionsUnordered class >> packageName [
	^ #'Collections-Unordered'
]
