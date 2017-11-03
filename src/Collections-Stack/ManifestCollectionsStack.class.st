"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsStack,
	#superclass : #PackageManifest,
	#category : #Collections-Stack
}

{ #category : #meta-data }
ManifestCollectionsStack class >> dependencies [
	^ #(#'Collections-Sequenceable')
]

{ #category : #meta-data }
ManifestCollectionsStack class >> description [
	^ 'I only contain a Stack implementation.'
]

{ #category : #meta-data }
ManifestCollectionsStack class >> packageName [
	^ #'Collections-Stack'
]
