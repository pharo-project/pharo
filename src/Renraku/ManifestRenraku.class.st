"
Morph and Model methods should access to the GUI theme via 'self theme' and not 'Smalltalk ui theme'.
"
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
