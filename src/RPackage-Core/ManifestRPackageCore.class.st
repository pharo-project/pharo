"
Manifest for RPackage-Core
"
Class {
	#name : #ManifestRPackageCore,
	#superclass : #PackageManifest,
	#category : #'RPackage-Core-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRPackageCore class >> manuallyResolvedDependencies [
	^ #(#UIManager)
]
