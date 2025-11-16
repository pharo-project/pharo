"
Includes the Monticello source code control system.
"
Class {
	#name : 'ManifestMonticello',
	#superclass : 'PackageManifest',
	#category : 'Monticello-Manifest',
	#package : 'Monticello',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestMonticello class >> ignoredDependencies [
	^ #(#'Graphics-Primitives')
]

{ #category : 'meta-data - dependency analyser' }
ManifestMonticello class >> manuallyResolvedDependencies [

	^ #( #'Graphics-Primitives' #Jobs )
]
