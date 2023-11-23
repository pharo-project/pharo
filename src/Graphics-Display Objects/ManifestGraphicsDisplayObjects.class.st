"
Package hosting graphics related display objects like Form and other
"
Class {
	#name : 'ManifestGraphicsDisplayObjects',
	#superclass : 'PackageManifest',
	#category : 'Graphics-Display Objects-Manifest',
	#package : 'Graphics-Display Objects',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestGraphicsDisplayObjects class >> manuallyResolvedDependencies [
	^ #( #'Collections-Streams')
]
