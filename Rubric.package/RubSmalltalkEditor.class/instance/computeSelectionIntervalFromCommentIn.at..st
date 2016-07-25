private
computeSelectionIntervalFromCommentIn: aString at: anInterval
	| comment commentInterval |
	comment := (RBScanner on: (aString copyFrom: anInterval first to: anInterval last) readStream) getComments.
	comment ifNil: [ ^ 0 to: -1 ].
	commentInterval := comment first.
	^ commentInterval first + 1 to: commentInterval last - 1