"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCodeImportCommandLineHandlers,
	#superclass : #PackageManifest,
	#category : 'CodeImportCommandLineHandlers'
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeImportCommandLineHandlers class >> manuallyResolvedDependencies [
	^ #(#UIManager #'Collections-Abstract' #'System-Support')
]
