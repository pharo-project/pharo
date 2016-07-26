remapping
remapTemp: aTemp toRemote: aRemote
	(temps removeKey: aTemp ifAbsent: [ #() ])
		do: [ :tempAccess |
			tempAccess name: aRemote.
			self rememberReference: tempAccess to: aRemote in: temps ].
		
	(remoteTemps removeKey: aTemp ifAbsent: [ #() ])
		do: [ :tempAccess |
			tempAccess tempVectorName: aRemote.
			self rememberReference: tempAccess to: aRemote in: remoteTemps. ].

	(closureCopiedValues removeKey: aTemp ifAbsent: [ #() ])
		do: [ :aClosureAndIndex | |closure index|
			closure := aClosureAndIndex key.
			index := aClosureAndIndex value.
			closure copiedValues at: index put: aRemote.
			closure tempMap at: aRemote put: (closure tempMap removeKey: aTemp).
			self rememberReference: aClosureAndIndex to: aRemote in: closureCopiedValues. ].