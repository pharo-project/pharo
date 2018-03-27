Class {
	#name : #ManifestSystemHashing,
	#superclass : #PackageManifest,
	#category : #'System-Hashing'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemHashing class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Platforms' #'OpalCompiler-Core' #'Transcript-Core' #CodeExport #'AST-Core' #'Collections-Abstract')

]
