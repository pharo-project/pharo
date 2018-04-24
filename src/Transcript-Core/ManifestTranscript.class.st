Class {
	#name : #ManifestTranscript,
	#superclass : #PackageManifest,
	#category : #'Transcript-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestTranscript class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Support')
]
