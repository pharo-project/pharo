"
Implementation for a new value holder
"
Class {
	#name : 'ManifestNewValueHolderCore',
	#superclass : 'PackageManifest',
	#category : 'NewValueHolder-Core-Manifest',
	#package : 'NewValueHolder-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestNewValueHolderCore class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]
