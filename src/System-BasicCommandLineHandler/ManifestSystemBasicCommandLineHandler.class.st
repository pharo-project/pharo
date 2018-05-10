Class {
	#name : #ManifestSystemBasicCommandLineHandler,
	#superclass : #PackageManifest,
	#category : #'System-BasicCommandLineHandler'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemBasicCommandLineHandler class >> manuallyResolvedDependencies [
	^ #( #'Collections-Strings' #'Collections-Abstract')
]
