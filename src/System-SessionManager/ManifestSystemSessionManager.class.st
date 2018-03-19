Class {
	#name : #ManifestSystemSessionManager,
	#superclass : #PackageManifest,
	#category : #'System-SessionManager'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSessionManager class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Support') 
]
