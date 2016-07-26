instance creation
on: aStream name: aFileName
	| class |
	class := self readerClassForFileNamed: aFileName.
	^ class
		ifNil: [self error: 'Unsupported format: ', aFileName]
		ifNotNil: [class on: aStream]