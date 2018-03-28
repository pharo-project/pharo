Class {
	#name : #ManifestCodeExport,
	#superclass : #PackageManifest,
	#category : #CodeExport
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeExport class >> manuallyResolvedDependencies [
	^ #(#'Slot-Core' #'System-Localization' #'FileSystem-Core' #'Collections-Abstract' #UIManager)
]
