"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCodeImport,
	#superclass : #PackageManifest,
	#category : 'CodeImport'
}

{ #category : #'meta-data - dependency analyser' }
ManifestCodeImport class >> manuallyResolvedDependencies [
	^ #(#'RPackage-Core' #'Collections-Abstract')
]
