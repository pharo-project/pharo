Class {
	#name : #ManifestSystemCommandLineHandler,
	#superclass : #PackageManifest,
	#category : #'System-CommandLineHandler'
}

{ #category : #'as yet unclassified' }
ManifestSystemCommandLineHandler class >> ignoredDependencies [
	^ #(#'System-Settings' #StartupPreferences)
]

{ #category : #'meta-data - dependency analyser' }
ManifestSystemCommandLineHandler class >> manuallyResolvedDependencies [
	^ #(#'FileSystem-Core' #'System-Settings' #'System-Support')
]
