Class {
	#name : #ManifestCodeImportCommandLineHandlers,
	#superclass : #PackageManifest,
	#category : #CodeImportCommandLineHandlers
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeImportCommandLineHandlers class >> manuallyResolvedDependencies [
	^ #(#UIManager #'Collections-Abstract' #'System-Support')
]
