accessing-defaults
creationTimeForFileName
	^ (self creationTime asString copyReplaceAll: ':' with: '-') copyReplaceAll: '.' with: '-'