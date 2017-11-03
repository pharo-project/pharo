"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestRandomCore,
	#superclass : #PackageManifest,
	#category : #Random-Core
}

{ #category : #'meta-data - dependency analyser' }
ManifestRandomCore class >> ignoredDependencies [
	^ #(#Transcript)
]

{ #category : #'meta-data - dependency analyser' }
ManifestRandomCore class >> manuallyResolvedDependencies [
	^ #(#'Transcript-Core')

]

{ #category : #'as yet unclassified' }
ManifestRandomCore class >> packageName [
	^ 'Random-Core'
]
