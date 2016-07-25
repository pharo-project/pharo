private - 1 writing
writeRemoteString: aString for: reference

	| remoteString |
	remoteString := 
		RemoteString 
			newString: aString 
			onFileNumber: self fileIndex 
			toFile: stream.

	remoteStringMap at: reference put: remoteString.

	^ remoteString