constructor
codeRemoteTemp: index remoteTemps: tempVector

	^(RemoteTempVectorNode new
		name: '_r', index printString
		index: index
		type: LdTempType
		scope: 0)
			remoteTemps: tempVector;
			yourself