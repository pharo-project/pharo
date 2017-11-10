"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestTextCore,
	#superclass : #PackageManifest,
	#category : #Text-Core
}

{ #category : #'meta-data - dependency analyser' }
ManifestTextCore class >> manuallyResolvedDependencies [
	^ #(#'Fonts-Abstract' #'AST-Core')
]
