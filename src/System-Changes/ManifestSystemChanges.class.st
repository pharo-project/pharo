"
Packacke with support for system changes recorded in changesets
"
Class {
	#name : 'ManifestSystemChanges',
	#superclass : 'PackageManifest',
	#category : 'System-Changes-Manifest',
	#package : 'System-Changes',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemChanges class >> manuallyResolvedDependencies [

	^ #( #'OpalCompiler-Core' #'FileSystem-Disk' #'Collections-Abstract' )
]
