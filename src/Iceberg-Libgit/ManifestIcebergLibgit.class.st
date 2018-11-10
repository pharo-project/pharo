"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestIcebergLibgit,
	#superclass : #PackageManifest,
	#category : #'Iceberg-Libgit-Manifest'
}

{ #category : #'meta-data' }
ManifestIcebergLibgit class >> description [ ^ 'I contains the core of the libgit binding of Iceberg. 

Libgit is a backend of Iceberg using the LibGit2 (https://libgit2.github.com/) to manage git communication. This package contains to code common to all export format. '
]
