"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestTranscript,
	#superclass : #PackageManifest,
	#category : #'Transcript-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestTranscript class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Support')
]
