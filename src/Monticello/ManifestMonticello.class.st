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
	^ #(#'Graphics-Primitives' #'FFI-Kernel' #'System-Settings-Core')
]

{ #category : 'meta-data - dependency analyser' }
ManifestMonticello class >> manuallyResolvedDependencies [

	^ #( #'Graphics-Primitives' #Jobs #'System-Settings-Core' )
]
