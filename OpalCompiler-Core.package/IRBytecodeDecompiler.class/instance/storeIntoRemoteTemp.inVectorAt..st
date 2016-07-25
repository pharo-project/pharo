instruction decoding
storeIntoRemoteTemp: remoteIndex inVectorAt: tempIndex 
	|  remoteArray remoteTempName |
	remoteArray := self scope tempAt: tempIndex.
	remoteTempName := self scope tempAt: remoteIndex inRemote: remoteArray.	
	irBuilder storeRemoteTemp: remoteTempName inVector: remoteArray.
	