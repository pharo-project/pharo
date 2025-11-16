"
Package to provide basic command line handling.
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
	^ #( #'System-Hashing')
]

{ #category : 'meta-data - dependency analyser' }
ManifestSystemCommandLineHandler class >> manuallyResolvedDependencies [
	^ #( #'Collections-Abstract' #'System-Support' #'Kernel-CodeModel' #'Collections-Streams' #'System-Hashing')
]
