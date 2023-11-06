"
Handling of Text and related text attributes
"
Class {
	#name : 'ManifestTextCore',
	#superclass : 'PackageManifest',
	#category : 'Text-Core-Manifest',
	#package : 'Text-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestTextCore class >> manuallyResolvedDependencies [
	^ #(#'Fonts-Abstract' #'AST-Core')
]
