"
Terminal / Command line support
"
Class {
	#name : 'ManifestSystemCommandLine',
	#superclass : 'PackageManifest',
	#category : 'System-CommandLine-Manifest',
	#package : 'System-CommandLine',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemCommandLine class >> manuallyResolvedDependencies [
	^ #(#'System-Support')
]
