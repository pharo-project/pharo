accessing selection
selectionRects
	^ Array
		streamContents: [ :strm | 
			strm nextPutAll: self paragraph selectionRects.
			self cursor ifNotNil: [ strm nextPut: self cursor bounds ] ]