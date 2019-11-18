"
Package for compression utilities like ZIP and other
"
Class {
	#name : #ManifestCompression,
	#superclass : #PackageManifest,
	#category : #'Compression-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestCompression class >> manuallyResolvedDependencies [
	^ #(#Jobs #'System-Support' #'System-Changes' #'Collections-Abstract')

]
