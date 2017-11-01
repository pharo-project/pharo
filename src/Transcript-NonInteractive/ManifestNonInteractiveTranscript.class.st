"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestNonInteractiveTranscript,
	#superclass : #PackageManifest,
	#category : 'Transcript-NonInteractive'
}

{ #category : #'meta-data - dependency analyser' }
ManifestNonInteractiveTranscript class >> manuallyResolvedDependencies [
	^ #(#'Collections-Strings' #'System-Support')
]
