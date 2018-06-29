"
Support package for VM events
"
Class {
	#name : #ManifestSystemVMEvents,
	#superclass : #PackageManifest,
	#category : #'System-VMEvents-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemVMEvents class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'Morphic-Core' #'Collections-Abstract' #'System-Platforms')
]
