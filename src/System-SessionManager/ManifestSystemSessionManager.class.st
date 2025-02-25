"
Image session management
"
Class {
	#name : 'ManifestSystemSessionManager',
	#superclass : 'PackageManifest',
	#category : 'System-SessionManager-Manifest',
	#package : 'System-SessionManager',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemSessionManager class >> manuallyResolvedDependencies [
	^ #(#'System-Support')
]
