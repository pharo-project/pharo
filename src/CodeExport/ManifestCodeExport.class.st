"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCodeExport,
	#superclass : #PackageManifest,
	#category : #CodeExport
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeExport class >> manuallyResolvedDependencies [
	^ #(#Slot #'System-Localization' #'FileSystem-Core' #'Collections-Abstract' #UIManager)
]
