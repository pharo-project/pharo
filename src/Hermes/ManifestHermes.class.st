"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestHermes,
	#superclass : #PackageManifest,
	#category : #Hermes
}

{ #category : #'meta-data - dependency analyser' }
ManifestHermes class >> manuallyResolvedDependencies [
	^ #(#'RPackage-Core' #Files #'Collections-Abstract')
]
