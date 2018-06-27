"
Unicode initialization 
"
Class {
	#name : #ManifestUnicodeInitialization,
	#superclass : #PackageManifest,
	#category : #'Unicode-Initialization-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestUnicodeInitialization class >> manuallyResolvedDependencies [
	^ #(#'Zinc-HTTP' #'Zinc-Resource-Meta-Core' #'AST-Core' #'Collections-Streams' #'Collections-Abstract')
]
