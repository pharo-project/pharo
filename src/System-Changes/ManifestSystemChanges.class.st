"
Package with support for system changes recorded. I mainly contains ChangeRecord that represents a change recorded on a file in fileOut format.
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
