menus
fileOutMessage
	"Put a description of the selected message on a file"

	| fileName |
	self selectedMessageName ifNotNil:
		[Cursor write showWhile:
			[self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].
		 ^self].
	items isEmpty ifTrue:
		[^self].
	fileName := UIManager default request: 'File out on which file?' initialAnswer: 'methods'.
	Cursor write showWhile:
		[| internalStream |
		internalStream := WriteStream on: (String new: 1000).
		internalStream header; timeStamp.
		items do:
			[:patchOp|
			patchOp definition isMethodDefinition ifTrue:
				[(patchOp definition actualClass notNil
				  and: [patchOp definition actualClass includesSelector: patchOp definition selector])
					ifTrue:
						[patchOp definition actualClass
							printMethodChunk: patchOp definition selector
							on: internalStream ]
					ifFalse:
						[internalStream nextChunkPut: patchOp definition className, ' removeSelector: ', patchOp definition selector printString]].
			patchOp definition isClassDefinition ifTrue:
				[patchOp definition actualClass
					ifNotNil:
						[internalStream nextChunkPut: patchOp definition actualClass definition.
						 patchOp definition comment ifNotNil:
							[patchOp definition actualClass organization
								putCommentOnFile: internalStream
								forClass: patchOp definition actualClass]]
					ifNil:
						[internalStream nextChunkPut: patchOp definition className, ' removeFromSystem']]].
		FileStream writeSourceCodeFrom: internalStream baseName: fileName isSt: true]