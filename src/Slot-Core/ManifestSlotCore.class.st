"
Core package for Slot support
"
Class {
	#name : 'ManifestSlotCore',
	#superclass : 'PackageManifest',
	#category : 'Slot-Core-Manifest',
	#package : 'Slot-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSlotCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Settings-Core')
]
