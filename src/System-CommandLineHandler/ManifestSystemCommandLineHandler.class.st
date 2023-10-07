"
Default command line handling
"
Class {
	#name : 'ManifestSystemCommandLineHandler',
	#superclass : 'PackageManifest',
	#category : 'System-CommandLineHandler-Manifest',
	#package : 'System-CommandLineHandler',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemCommandLineHandler class >> ignoredDependencies [
	^ #(#'System-Settings-Core' #StartupPreferences)
]

{ #category : 'meta-data - dependency analyser' }
ManifestSystemCommandLineHandler class >> manuallyResolvedDependencies [
	^ #(#'FileSystem-Core' #'System-Settings-Core' #'System-Support')
]
