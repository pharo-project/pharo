"
Package providing Random Number generation
"
Class {
	#name : 'ManifestRandomCore',
	#superclass : 'PackageManifest',
	#category : 'Random-Core-Manifest',
	#package : 'Random-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestRandomCore class >> ignoredDependencies [
	^ #(#Transcript)
]

{ #category : 'accessing' }
ManifestRandomCore class >> packageName [
	^ 'Random-Core'
]
