instructions
pushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	stack push.
	encoder genPushRemoteTemp: tempIndex - 1 inVectorAt: tempVectorIndex - 1