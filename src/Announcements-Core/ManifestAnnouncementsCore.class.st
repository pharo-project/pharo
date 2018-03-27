Class {
	#name : #ManifestAnnouncementsCore,
	#superclass : #PackageManifest,
	#category : #'Announcements-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestAnnouncementsCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
