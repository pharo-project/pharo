"
Primary package for Transcript related implementation
"
Class {
	#name : #ManifestTranscript,
	#superclass : #PackageManifest,
	#category : #'Transcript-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestTranscript class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Support')
]
