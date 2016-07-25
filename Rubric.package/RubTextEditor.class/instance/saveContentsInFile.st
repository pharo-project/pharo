menu messages
saveContentsInFile
	"Save the receiver's contents string to a file, prompting the user for a file-name.  Suggest a reasonable file-name."

	| fileName stringToSave parentWindow labelToUse suggestedName lastIndex |
	self flag: #beurk.

	stringToSave := self string. 
	stringToSave size = 0 ifTrue: [^self inform: 'nothing to save.'].
	parentWindow := self model dependents 
				detect: [:dep | dep isKindOf: SystemWindow]
				ifNone: [nil].
	labelToUse := parentWindow ifNil: ['Untitled']
				ifNotNil: [parentWindow label].
	suggestedName := nil.
	#(#('Decompressed contents of: ' '.gz')) do: 
			[:leaderTrailer | 
			"can add more here..."

			(labelToUse beginsWith: leaderTrailer first) 
				ifTrue: 
					[suggestedName := labelToUse copyFrom: leaderTrailer first size + 1
								to: labelToUse size.
					(labelToUse endsWith: leaderTrailer last) 
						ifTrue: 
							[suggestedName := suggestedName copyFrom: 1
										to: suggestedName size - leaderTrailer last size]
						ifFalse: 
							[lastIndex := suggestedName lastIndexOf: $. ifAbsent: [0].
							(lastIndex = 0 or: [lastIndex = 1]) 
								ifFalse: [suggestedName := suggestedName copyFrom: 1 to: lastIndex - 1]]]].
	suggestedName ifNil: [suggestedName := labelToUse , '.text'].
	fileName := UIManager default request: 'File name?'
				initialAnswer: suggestedName.
	fileName isEmptyOrNil 
		ifFalse: 
			[(FileStream newFileNamed: fileName)
				nextPutAll: stringToSave;
				close]