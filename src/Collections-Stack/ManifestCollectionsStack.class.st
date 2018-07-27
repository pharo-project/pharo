"
I only contain a Stack implementation.
"
Class {
	#name : #ManifestCollectionsStack,
	#superclass : #PackageManifest,
	#category : #'Collections-Stack-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsStack class >> dependencies [
	^ #(#'Collections-Sequenceable')
]

{ #category : #'meta-data' }
ManifestCollectionsStack class >> packageName [
	^ #'Collections-Stack'
]
