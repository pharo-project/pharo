accessing
commitID: aString
	commitID := ByteArray readHexFrom: aString.
	shortCommitID := (ByteArray readHexFrom: (aString copyFrom: 1 to: 7)) asInteger