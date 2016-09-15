private
nextEntryFromPosition: aFilePosition

	^ self readEntriesWith: [ :readStream | 
		[ readStream position: aFilePosition.
			self entryReader nextEntryFrom: readStream ]
				on: Error 
				do: [ :error | 
					self 
						readingError: error 
						on: self fileReference 
						position: aFilePosition ] ]