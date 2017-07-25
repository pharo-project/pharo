printing
printOn: aStream

	aStream nextPutAll: ((self method hasSourceCode
		or: [ "There is a decompiler"
			Smalltalk globals includesKey: #FBDDecompiler ])
				ifTrue: [ self sourceNode formattedCode ]
				ifFalse: [ 'aBlockClosure(no source code or decompiler available)' ])