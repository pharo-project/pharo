private
versionDoesNotExistError: versionStringOrSymbol

	((Smalltalk at: #MetacelloSymbolicVersionDoesNotExistError) project: self project versionString: versionStringOrSymbol) signal