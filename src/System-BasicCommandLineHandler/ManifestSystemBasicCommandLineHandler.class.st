"
Package to provide basic command line handling.
"
Class {
	#name : 'ManifestSystemBasicCommandLineHandler',
	#superclass : 'PackageManifest',
	#category : 'System-BasicCommandLineHandler-Manifest',
	#package : 'System-BasicCommandLineHandler',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemBasicCommandLineHandler class >> manuallyResolvedDependencies [
	^ #( #'Collections-Strings' #'Collections-Abstract')
]
