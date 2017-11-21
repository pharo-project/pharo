"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSlot,
	#superclass : #PackageManifest,
	#category : #Slot
}

{ #category : #'as yet unclassified' }
ManifestSlot class >> ignoredDependencies [
	^ #(#'System-Settings')
]

{ #category : #'meta-data - dependency analyser' }
ManifestSlot class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Settings' #'OpalCompiler-Core')
]
