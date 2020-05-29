"
Core package for Slot support
"
Class {
	#name : #ManifestSlotCore,
	#superclass : #PackageManifest,
	#category : #'Slot-Core-Manifest'
}

{ #category : #'as yet unclassified' }
ManifestSlotCore class >> ignoredDependencies [
	^ #(#'System-Settings-Core')
]

{ #category : #'meta-data - dependency analyser' }
ManifestSlotCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Settings-Core')
]
