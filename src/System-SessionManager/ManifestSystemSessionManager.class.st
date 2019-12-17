"
Image session management
"
Class {
	#name : #ManifestSystemSessionManager,
	#superclass : #PackageManifest,
	#category : #'System-SessionManager-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSessionManager class >> manuallyResolvedDependencies [
	^ #(#'System-Support') 
]
