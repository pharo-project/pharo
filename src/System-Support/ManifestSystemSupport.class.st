Class {
	#name : #ManifestSystemSupport,
	#superclass : #PackageManifest,
	#category : #'System-Support'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSupport class >> ignoredDependencies [
	^ #(#'System-Settings' #'Graphics-Primitives')
]

{ #category : #'meta-data - dependency analyser' }
ManifestSystemSupport class >> manuallyResolvedDependencies [
	^ #(#'Collections-Support' #'System-Localization' #'System-Settings' #'System-Platforms')
]
