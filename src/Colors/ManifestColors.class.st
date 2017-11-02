"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestColors,
	#superclass : #PackageManifest,
	#category : 'Colors'
}

{ #category : #'meta-data - dependency analyser' }
ManifestColors class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'AST-Core')
]
