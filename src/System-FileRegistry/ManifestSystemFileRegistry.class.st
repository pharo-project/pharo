"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemFileRegistry,
	#superclass : #PackageManifest,
	#category : #'System-FileRegistry'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemFileRegistry class >> manuallyResolvedDependencies [
	^ #(#'System-Localization' #'FileSystem-Core' #'Collections-Abstract')
]
