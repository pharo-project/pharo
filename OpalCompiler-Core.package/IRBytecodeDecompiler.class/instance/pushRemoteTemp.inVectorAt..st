instruction decoding
pushRemoteTemp: remoteIndex inVectorAt: tempIndex 
	|  remoteArray remoteTempName |
	remoteArray := self scope tempAt: tempIndex.
	remoteTempName := self scope tempAt: remoteIndex inRemote: remoteArray.
	irBuilder pushRemoteTemp: remoteTempName inVector: remoteArray.