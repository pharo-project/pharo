"
Provide service registration to handle specific file extension
"
Class {
	#name : 'ManifestSystemFileRegistry',
	#superclass : 'PackageManifest',
	#category : 'System-FileRegistry-Manifest',
	#package : 'System-FileRegistry',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemFileRegistry class >> manuallyResolvedDependencies [
	^ #(#'FileSystem-Core' #'Collections-Abstract')
]
