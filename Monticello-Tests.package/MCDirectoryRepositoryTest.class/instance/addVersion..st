actions
addVersion: aVersion
	| file |
	file := (directory / aVersion fileName) asFileReference writeStream.
	aVersion fileOutOn: file.
	file close.