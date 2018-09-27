"
Package providing Random Number generation
"
Class {
	#name : #ManifestRandomCore,
	#superclass : #PackageManifest,
	#category : #'Random-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRandomCore class >> ignoredDependencies [
	^ #(#Transcript)
]

{ #category : #'meta-data - dependency analyser' }
ManifestRandomCore class >> manuallyResolvedDependencies [
	^ #(#'Transcript-Core')

]

{ #category : #accessing }
ManifestRandomCore class >> packageName [
	^ 'Random-Core'
]
