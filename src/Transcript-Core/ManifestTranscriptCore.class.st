"
Primary package for Transcript related implementation
"
Class {
	#name : 'ManifestTranscriptCore',
	#superclass : 'PackageManifest',
	#category : 'Transcript-Core-Manifest',
	#package : 'Transcript-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestTranscriptCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Support')
]
