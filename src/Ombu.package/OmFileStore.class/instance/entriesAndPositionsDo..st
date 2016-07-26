enumerating
entriesAndPositionsDo: aBlockClosure 
	"Evaluate the closure on each entry and its position inf the file. Arguments are optional since #cull:cull: is used."

	self fileReference exists ifFalse: [ ^self ].

	self readEntriesWith: [ :entryReader :readStream | 
		[ readStream atEnd ] whileFalse: [ | filePosition entry |

			[ filePosition := readStream position.
			entry := entryReader nextEntryFrom: readStream.

			aBlockClosure cull: entry cull: filePosition.
	
			] on: Error do: [ :error | 
				self 
					error: error
					reading: self fileReference
					position: filePosition ] ] ]