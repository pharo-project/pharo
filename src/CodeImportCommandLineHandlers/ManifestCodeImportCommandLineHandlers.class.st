"
A package containing command line handlers to import code.
"
Class {
	#name : 'ManifestCodeImportCommandLineHandlers',
	#superclass : 'PackageManifest',
	#category : 'CodeImportCommandLineHandlers-Manifest',
	#package : 'CodeImportCommandLineHandlers',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestCodeImportCommandLineHandlers class >> manuallyResolvedDependencies [
	^ #(#UIManager #'Collections-Abstract' #'System-Support')
]
