Class {
	#name : #ManifestRenraku,
	#superclass : #PackageManifest,
	#category : #'Renraku-Manifest'
}

{ #category : #release }
ManifestRenraku class >> preUnload [

	ReSettings inspectorPluggin: false.
	ReSettings spotterPlugin: false.
]
