"
Package for hosting classes to provide non-interactive Transcript (for headless scenario or other)
"
Class {
	#name : #ManifestNonInteractiveTranscript,
	#superclass : #PackageManifest,
	#category : #'Transcript-NonInteractive-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestNonInteractiveTranscript class >> manuallyResolvedDependencies [
	^ #(#'Collections-Strings' #'System-Support')
]
