accessing
getVersionsForPackageDirectoryName: packageDirectoryName
	| commitIDs entryVersions entryVersionIndex |
	entryVersions := OrderedCollection new.
	entryVersionIndex := IdentityDictionary new.
	commitIDs := self gitVersionsForPackage: packageDirectoryName.
	[ commitIDs atEnd ]
		whileFalse: [ 
			| logSize commitID author position parents message dateAndTime newPackageEntry |
			(commitIDs match: 'log size ')
				ifFalse: [ self error: 'Unable to find git data in input' ].
			logSize := (commitIDs upTo: $|) trimBoth asInteger.
			position := commitIDs position.
			commitID := (commitIDs upTo: $|) trimBoth.
			author := (commitIDs upTo: $|) onlyLetters.
			dateAndTime := DateAndTime fromString: (commitIDs upTo: $|).
			parents := ((commitIDs upTo: $|) trimBoth substrings
				collect: [ :parent | entryVersionIndex at: (ByteArray readHexFrom: (parent copyFrom: 1 to: 7)) asInteger ])
				asOrderedCollection.
			message := (commitIDs next: logSize + position - commitIDs position) convertFromSystemString.
			newPackageEntry := GitFileTreePackageEntry new
				packageDirectory: packageDirectoryName;
				commitID: commitID;
				author: author;
				message: message;
				timeStamp: dateAndTime;
				parents: parents;
				yourself.
			entryVersionIndex at: newPackageEntry shortCommitID put: newPackageEntry.
			entryVersions add: newPackageEntry ].
	entryVersions doWithIndex: [ :entryVersion :version | entryVersion setMCVersionInfoWithNumber: version ].
	^ entryVersions