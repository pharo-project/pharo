"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemAnnouncements,
	#superclass : #PackageManifest,
	#category : 'System-Announcements'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemAnnouncements class >> ignoredDependencies [
	^ #(#PragmaCollector)
]

{ #category : #'meta-data - dependency analyser' }
ManifestSystemAnnouncements class >> manuallyResolvedDependencies [
	^ #(#'Transcript-Core')

]
