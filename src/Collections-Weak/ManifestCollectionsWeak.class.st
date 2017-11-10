"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsWeak,
	#superclass : #PackageManifest,
	#category : #Collections-Weak
}

{ #category : #meta-data }
ManifestCollectionsWeak class >> dependencies [
	^ #(#'System-Finalization' #'Collections-Support' #'Collections-Sequenceable' #'Collections-Unordered' #'Collections-Abstract' #Kernel)
]

{ #category : #meta-data }
ManifestCollectionsWeak class >> description [
	^ 'All weak collections'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsWeak class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'Collections-Streams')
]

{ #category : #meta-data }
ManifestCollectionsWeak class >> packageName [
	^ #'Collections-Weak'
]
