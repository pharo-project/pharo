file-out
writeSourceCodeFrom: aStream baseName: baseName isSt: stOrCsFlag

	| extension converter fileName  |
	extension := stOrCsFlag ifTrue: ['.st']  ifFalse: ['.cs'].
	fileName := baseName, extension.
	fileName := FileSystem disk checkName: fileName fixErrors: true.
	(ZnCrPortableWriteStream on: (ZnCharacterWriteStream
		on: (File named: fileName) writeStream
		encoding: 'utf8')) nextPutAll: aStream contents.

	self inform: 'Filed out to: ', String cr, fileName.