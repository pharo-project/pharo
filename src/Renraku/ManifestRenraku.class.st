"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestRenraku,
	#superclass : #PackageManifest,
	#category : #Renraku-Manifest
}

{ #category : #release }
ManifestRenraku class >> preUnload [

	ReSettings inspectorPluggin: false.
	ReSettings spotterPlugin: false.
]
