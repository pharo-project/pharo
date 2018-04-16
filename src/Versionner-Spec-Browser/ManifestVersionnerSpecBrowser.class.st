Class {
	#name : #ManifestVersionnerSpecBrowser,
	#superclass : #PackageManifest,
	#category : #'Versionner-Spec-Browser'
}

{ #category : #release }
ManifestVersionnerSpecBrowser class >> preUnload [

	VersionnerSpecBrowser clean.
]
