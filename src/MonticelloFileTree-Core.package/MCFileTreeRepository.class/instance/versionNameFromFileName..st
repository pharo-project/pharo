accessing
versionNameFromFileName: aString
	| description |
	description := self packageDescriptionFromPackageDirectory: (self fileDirectoryOn: aString).
	^ description first , '-' , description second , '.' , description third printString