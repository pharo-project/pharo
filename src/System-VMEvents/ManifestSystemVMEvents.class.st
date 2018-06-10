Class {
	#name : #ManifestSystemVMEvents,
	#superclass : #PackageManifest,
	#category : #'System-VMEvents'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemVMEvents class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'Collections-Abstract' #'System-Platforms')
]
