Extension { #name : #IceChangeImporter }

{ #category : #'*Iceberg-Libgit' }
IceChangeImporter >> visitGitChange: anIceGitChange [ 

	| importer |
	importer := IceGitChangeImporter new
		path: anIceGitChange path;
		diff: diff;
		version: version;
		yourself.
	importer importOn: parentNode.
]
