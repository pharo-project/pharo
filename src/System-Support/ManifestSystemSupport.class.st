"
Support classes to manage the system
"
Class {
	#name : #ManifestSystemSupport,
	#superclass : #PackageManifest,
	#category : #'System-Support-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSupport class >> ignoredDependencies [
	^ #(#'System-Settings' #'Graphics-Primitives')
]

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSupport class >> manuallyResolvedDependencies [
	^ #(#'Collections-Support' #'System-Localization' #'System-Settings' #'System-Platforms')
]
