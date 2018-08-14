"
Class extensions on other Pharo packages in order to give them access to mappings of Unicode values 
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
