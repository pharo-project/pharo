accessing
versionNameFromFileName: aFileName
	^ (aFileName copyUpToLast: $.) copyUpTo: $(