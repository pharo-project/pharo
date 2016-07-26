i/o
writeStreamForFileNamed: aString replace: ignoreBoolean do: aBlock
	| entity |
	entity := self entityStreamContents: aBlock.
	self displayProgress: 'Uploading ', aString during: [
		self httpClient
			entity: entity;
			ifFail: [ :exception | 
				(exception isKindOf: ZnHttpUnsuccessful) 
					ifTrue: [ ^ self handleUnsuccessfulResponse: exception response ].
				self error: 'Could not save ', aString, ': ', exception printString ];
			url: (self urlForFileNamed: aString);
			put ]