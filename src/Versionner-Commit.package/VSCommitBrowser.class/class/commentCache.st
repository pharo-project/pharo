accessing
commentCache
	^ commentCache ifNil: [ commentCache := HistoryCollection new maxSize: self maxCommentCache ]