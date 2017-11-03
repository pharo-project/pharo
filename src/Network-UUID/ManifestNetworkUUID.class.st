"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestNetworkUUID,
	#superclass : #PackageManifest,
	#category : #Network-UUID
}

{ #category : #'meta-data - dependency analyser' }
ManifestNetworkUUID class >> ignoredDependencies [
	^ #(#'Network-Kernel')
]

{ #category : #'meta-data - dependency analyser' }
ManifestNetworkUUID class >> manuallyResolvedDependencies [
	^ #(#'AST-Core' #'Collections-Abstract')
]
