Class {
	#name : #ManifestSystemAnnouncements,
	#superclass : #PackageManifest,
	#category : #'System-Announcements'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemAnnouncements class >> ignoredDependencies [
	^ #(#PragmaCollector)
]

{ #category : #'meta-data - dependency analyser' }
ManifestSystemAnnouncements class >> manuallyResolvedDependencies [
	^ #(#'Transcript-Core')

]
