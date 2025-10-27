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
ManifestSystemBasicCommandLineHandler class >> ignoredDependencies [
	^ #(#'System-Settings-Core' #StartupPreferences #'System-Hashing')
]

{ #category : 'meta-data - dependency analyser' }
ManifestSystemBasicCommandLineHandler class >> manuallyResolvedDependencies [
	^ #( #'Collections-Abstract' #'FileSystem-Core' #'System-Support' #'Kernel-CodeModel' #'System-Settings-Core' #'Collections-Streams' #'System-Hashing')
]
