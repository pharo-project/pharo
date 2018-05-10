"
All weak collections
"
Class {
	#name : #ManifestCollectionsWeak,
	#superclass : #PackageManifest,
	#category : #'Collections-Weak-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsWeak class >> dependencies [
	^ #(#'System-Finalization' #'Collections-Support' #'Collections-Sequenceable' #'Collections-Unordered' #'Collections-Abstract' #Kernel)
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsWeak class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'Collections-Streams')
]

{ #category : #'meta-data' }
ManifestCollectionsWeak class >> packageName [
	^ #'Collections-Weak'
]
