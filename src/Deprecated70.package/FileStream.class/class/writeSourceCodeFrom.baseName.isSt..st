*Deprecated70
writeSourceCodeFrom: aStream baseName: baseName isSt: stOrCsFlag

	| extension converter fileName  |
	[ 	extension :=  stOrCsFlag ifTrue: ['.st']  ifFalse: ['.cs'].
		converter := aStream contents isAsciiString 
			ifTrue: [MacRomanTextConverter new] 
			ifFalse: [UTF8TextConverter new].
		fileName := baseName, extension.
		fileName := FileSystem disk checkName: fileName fixErrors: true.
		[FileStream newFileNamed: fileName do: [:fileStream |
			fileName := fileStream name. "in case it is changed when handling FileExistsException"
			(converter isMemberOf: UTF8TextConverter)
				ifTrue: [fileStream binary.
						UTF8TextConverter writeBOMOn: fileStream].
			fileStream
				text;
				converter: converter;
				nextPutAll: aStream contents;
				close]] on: Abort do: [:e | ^self ]
	] on: Error do: 
	[ 	:error | 
		error isResumable 
			ifTrue: [ error resumeUnchecked: error defaultAction]
			ifFalse: [ error pass ]
	].
	self inform: 'Filed out to: ', String cr, fileName.