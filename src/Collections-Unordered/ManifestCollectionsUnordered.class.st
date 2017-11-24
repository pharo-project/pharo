"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsUnordered,
	#superclass : #PackageManifest,
	#category : #'Collections-Unordered'
}

{ #category : #'meta-data' }
ManifestCollectionsUnordered class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Support' #Kernel #'Collections-Sequenceable' #'Collections-Abstract')
]

{ #category : #'meta-data' }
ManifestCollectionsUnordered class >> description [
	^ 'I contain collections that do not care about the order of the elements they contain.
Main classes: Bag, Dictionary, Matrix, Set'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsUnordered class >> manuallyResolvedDependencies [
	^ #(#Jobs #'Collections-Streams' #UIManager #'System-Support')
]

{ #category : #'meta-data' }
ManifestCollectionsUnordered class >> packageName [
	^ #'Collections-Unordered'
]
