"
Support for source and changes files
"
Class {
	#name : 'ManifestSystemSources',
	#superclass : 'PackageManifest',
	#category : 'System-Sources-Manifest',
	#package : 'System-Sources',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemSources class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Announcements-Core' #'System-Support')
]
