instance creation
on: aStream fileName: aFileName
	| reader |
	reader := self on: aStream.
	reader fileName: aFileName.
	^reader