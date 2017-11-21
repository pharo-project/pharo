"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemCommandLine,
	#superclass : #PackageManifest,
	#category : #System-CommandLine
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemCommandLine class >> manuallyResolvedDependencies [
	^ #(#'System-Support')
]
