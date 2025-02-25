"
Package manifest for package metadata
"
Class {
	#name : 'ManifestCodeImport',
	#superclass : 'PackageManifest',
	#category : 'CodeImport-Manifest',
	#package : 'CodeImport',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestCodeImport class >> manuallyResolvedDependencies [

	^ #( #'Collections-Abstract' )
]
