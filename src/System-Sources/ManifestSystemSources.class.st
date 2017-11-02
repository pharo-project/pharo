"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemSources,
	#superclass : #PackageManifest,
	#category : 'System-Sources'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSources class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #Jobs #'Announcements-Core' #'Ring-Core-Kernel' #CodeExport #DeprecatedFileStream #'Collections-Abstract')
]
