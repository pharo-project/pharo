"
A package to provide the Pharo default command line handler
"
Class {
	#name : #ManifestSystemCommandLineHandler,
	#superclass : #PackageManifest,
	#category : #'System-CommandLineHandler-Manifest'
}

{ #category : #'as yet unclassified' }
ManifestSystemCommandLineHandler class >> ignoredDependencies [
	^ #(#'System-Settings' #StartupPreferences)
]

{ #category : #'meta-data - dependency analyser' }
ManifestSystemCommandLineHandler class >> manuallyResolvedDependencies [
	^ #(#'FileSystem-Core' #'System-Settings' #'System-Support')
]
