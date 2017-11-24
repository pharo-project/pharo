"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestVersionnerSpecBrowser,
	#superclass : #PackageManifest,
	#category : #'Versionner-Spec-Browser'
}

{ #category : #release }
ManifestVersionnerSpecBrowser class >> preUnload [

	VersionnerSpecBrowser clean.
]
