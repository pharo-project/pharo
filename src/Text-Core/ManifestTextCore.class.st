Class {
	#name : #ManifestTextCore,
	#superclass : #PackageManifest,
	#category : #'Text-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestTextCore class >> manuallyResolvedDependencies [
	^ #(#'Fonts-Abstract' #'AST-Core')
]
