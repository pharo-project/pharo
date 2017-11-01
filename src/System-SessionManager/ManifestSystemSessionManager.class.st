"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemSessionManager,
	#superclass : #PackageManifest,
	#category : 'System-SessionManager'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSessionManager class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Support') 
]
