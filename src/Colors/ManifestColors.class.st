Class {
	#name : #ManifestColors,
	#superclass : #PackageManifest,
	#category : #Colors
}

{ #category : #'meta-data - dependency analyser' }
ManifestColors class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'AST-Core')
]
