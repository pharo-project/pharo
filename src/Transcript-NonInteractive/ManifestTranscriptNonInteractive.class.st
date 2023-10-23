"
Package for hosting classes to provide non-interactive Transcript (for headless scenario or other)
"
Class {
	#name : 'ManifestTranscriptNonInteractive',
	#superclass : 'PackageManifest',
	#category : 'Transcript-NonInteractive-Manifest',
	#package : 'Transcript-NonInteractive',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestTranscriptNonInteractive class >> manuallyResolvedDependencies [
	^ #(#'Collections-Strings' #'System-Support')
]
