querying
versionOrNil

	^[ self version ] on: MetacelloVersionDoesNotExistError do: [:ex | ^nil ].