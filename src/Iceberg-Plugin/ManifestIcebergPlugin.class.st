"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestIcebergPlugin,
	#superclass : #PackageManifest,
	#category : #'Iceberg-Plugin-Manifest'
}

{ #category : #'meta-data' }
ManifestIcebergPlugin class >> description [ ^ 'I contains a plugin manager for Iceberg. 

This plugin manager allow to extends Iceberg''s functionnalities with modularity.'
]
