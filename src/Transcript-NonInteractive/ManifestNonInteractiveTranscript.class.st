Class {
	#name : #ManifestNonInteractiveTranscript,
	#superclass : #PackageManifest,
	#category : #'Transcript-NonInteractive'
}

{ #category : #'meta-data - dependency analyser' }
ManifestNonInteractiveTranscript class >> manuallyResolvedDependencies [
	^ #(#'Collections-Strings' #'System-Support')
]
