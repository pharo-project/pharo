"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestSystemLocalization,
	#superclass : #PackageManifest,
	#category : 'System-Localization'
}

{ #category : #'meta-data - dependency analyser' }
ManifestSystemLocalization class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams' #'System-Platforms' #'Collections-Abstract')
]
