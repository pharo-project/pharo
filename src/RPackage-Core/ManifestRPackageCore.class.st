"
Manifest for RPackage-Core
"
Class {
	#name : #ManifestRPackageCore,
	#superclass : #PackageManifest,
	#category : #'RPackage-Core'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRPackageCore class >> manuallyResolvedDependencies [
	^ #(#'Ring-Deprecated-Core-Kernel' #UIManager #'System-Sources')
]
