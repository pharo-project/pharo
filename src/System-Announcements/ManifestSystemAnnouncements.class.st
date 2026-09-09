"
Package to provide system wide announcements for Pharo image
"
Class {
	#name : 'ManifestSystemAnnouncements',
	#superclass : 'PackageManifest',
	#category : 'System-Announcements-Manifest',
	#package : 'System-Announcements',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemAnnouncements class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Transcript-NonInteractive')
]
