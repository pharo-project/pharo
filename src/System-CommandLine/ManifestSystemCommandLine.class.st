Class {
	#name : #ManifestSystemCommandLine,
	#superclass : #PackageManifest,
	#category : #'System-CommandLine'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemCommandLine class >> manuallyResolvedDependencies [
	^ #(#'System-Support')
]
