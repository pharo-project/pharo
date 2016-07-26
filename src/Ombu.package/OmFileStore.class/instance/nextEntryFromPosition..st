private
nextEntryFromPosition: aFilePosition

	^ self readEntriesWith: [ :entryReader :readStream | 
		[ readStream position: aFilePosition.
			self newEntryReader nextEntryFrom: readStream ]
				on: Error 
				do: [ :error | 
					self 
						error: error 
						reading: self fileReference 
						position: aFilePosition ] ]