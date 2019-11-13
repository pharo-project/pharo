"
Package to provide system wide announcements for Pharo image
"
Class {
	#name : #ManifestSystemAnnouncements,
	#superclass : #PackageManifest,
	#category : #'System-Announcements-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemAnnouncements class >> ignoredDependencies [
	^ #(#PragmaCollector)
]
