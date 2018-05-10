"
Package manifest for package metadata
"
Class {
	#name : #ManifestCodeImport,
	#superclass : #PackageManifest,
	#category : #'CodeImport-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeImport class >> manuallyResolvedDependencies [
	^ #(#'RPackage-Core' #'Collections-Abstract')
]
