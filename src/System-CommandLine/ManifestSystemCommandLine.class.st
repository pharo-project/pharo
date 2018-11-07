"
Terminal / Command line support
"
Class {
	#name : #ManifestSystemCommandLine,
	#superclass : #PackageManifest,
	#category : #'System-CommandLine-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemCommandLine class >> manuallyResolvedDependencies [
	^ #(#'System-Support')
]
