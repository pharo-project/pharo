visiting
visitPushRemoteTemp: remoteTemp
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [:each | each name = remoteTemp tempVectorName].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen pushRemoteTemp: tempVectorIndex inVectorAt: tempIndex
