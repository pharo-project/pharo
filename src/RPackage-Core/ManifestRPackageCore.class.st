"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestRPackageCore,
	#superclass : #PackageManifest,
	#category : 'RPackage-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRPackageCore class >> manuallyResolvedDependencies [
	^ #(#'Ring-Core-Kernel' #UIManager #'System-Sources')
]
