Class {
	#name : #ManifestCodeImport,
	#superclass : #PackageManifest,
	#category : #CodeImport
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeImport class >> manuallyResolvedDependencies [
	^ #(#'RPackage-Core' #'Collections-Abstract')
]
