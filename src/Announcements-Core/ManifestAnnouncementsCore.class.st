"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestAnnouncementsCore,
	#superclass : #PackageManifest,
	#category : #'Announcements-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestAnnouncementsCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Abstract')
]
