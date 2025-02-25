"
Localization support
"
Class {
	#name : 'ManifestSystemLocalization',
	#superclass : 'PackageManifest',
	#category : 'System-Localization-Manifest',
	#package : 'System-Localization',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemLocalization class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Platforms' #'Collections-Abstract')
]
