"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestNECompletion,
	#superclass : #PackageManifest,
	#category : #'NECompletion-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestNECompletion class >> manuallyResolvedDependencies [
	^ #(#'Tool-Registry' #'OpalCompiler-Core' #'Tool-Base' #'Text-Core' #'Collections-Abstract')
]
