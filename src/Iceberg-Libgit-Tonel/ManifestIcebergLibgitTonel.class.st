"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestIcebergLibgitTonel,
	#superclass : #PackageManifest,
	#category : #'Iceberg-Libgit-Tonel-Manifest'
}

{ #category : #'meta-data' }
ManifestIcebergLibgitTonel class >> description [ ^ 'I contain the libgit binding for the Tonel export format. 

Iceberg can manage different export format and Tonel is one of them. It will export with a file per class format. The cons of this format is that it is not easy to get a fine grained history. The pros of this format is that it will create bigger files than FileTree. When a project contains a lot of small file it takes much more space on the disk because of the file system''s blocs minimum space. Tonel also reduce the troubles on windows because of the long files names that can produce FileTree. '
]
