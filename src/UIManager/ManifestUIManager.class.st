"
Package for the UI Manager and related classes
"
Class {
	#name : #ManifestUIManager,
	#superclass : #PackageManifest,
	#category : #'UIManager-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestUIManager class >> ignoredDependencies [
	^ #(#CodeImport #'FileSystem-Core' #Files #'Graphics-Display Objects' #'Transcript-NonInteractive' #'System-Settings-Core')
]

{ #category : #'meta-data - dependency analyser' }
ManifestUIManager class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Support' #'System-Settings-Core')
]
