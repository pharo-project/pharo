Class {
	#name : #ManifestGraphicsDisplayObjects,
	#superclass : #PackageManifest,
	#category : #'Graphics-Display Objects'
}

{ #category : #'meta-data - dependency analyser' }
ManifestGraphicsDisplayObjects class >> manuallyResolvedDependencies [
	^ #(#'Graphics-Canvas' #'Polymorph-Widgets' #'Graphics-Files' #'Collections-Streams')
]
