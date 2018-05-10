Class {
	#name : #ManifestSystemFileRegistry,
	#superclass : #PackageManifest,
	#category : #'System-FileRegistry'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemFileRegistry class >> manuallyResolvedDependencies [
	^ #(#'System-Localization' #'FileSystem-Core' #'Collections-Abstract')
]
