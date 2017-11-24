"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsSupport,
	#superclass : #PackageManifest,
	#category : #'Collections-Support'
}

{ #category : #'meta-data' }
ManifestCollectionsSupport class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Native' #'Collections-Unordered' #Kernel #'Collections-Abstract')
]

{ #category : #'meta-data' }
ManifestCollectionsSupport class >> description [
	^ 'Some basic classes used in collections: Link, Associtation, Weak*, CharacterSet, SetElement, etc.'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsSupport class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]

{ #category : #'meta-data' }
ManifestCollectionsSupport class >> packageName [
	^ #'Collections-Support'
]
