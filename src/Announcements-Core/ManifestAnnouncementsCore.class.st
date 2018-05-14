"
A package with classes for the announcement framwork - an event notification framework.
"
Class {
	#name : #ManifestAnnouncementsCore,
	#superclass : #PackageManifest,
	#category : #'Announcements-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestAnnouncementsCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
