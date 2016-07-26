versions
version: aVersionString ifAbsent: aBlock

	^[ self version: aVersionString ] on: MetacelloVersionDoesNotExistError do: [:ex | aBlock value ].