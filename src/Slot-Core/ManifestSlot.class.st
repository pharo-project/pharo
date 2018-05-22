Class {
	#name : #ManifestSlot,
	#superclass : #PackageManifest,
	#category : #'Slot-Core-Manifest'
}

{ #category : #'as yet unclassified' }
ManifestSlot class >> ignoredDependencies [
	^ #(#'System-Settings')
]

{ #category : #'meta-data - dependency analyser' }
ManifestSlot class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Settings' #'OpalCompiler-Core')
]
