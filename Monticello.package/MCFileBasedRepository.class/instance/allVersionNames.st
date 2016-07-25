accessing
allVersionNames
	^ self readableFileNames collect: [:ea | self versionNameFromFileName: ea]