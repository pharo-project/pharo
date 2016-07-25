accessing segments
segmentsAtLine: aLineNumber
	^ self segments select: [ :s | s firstLineIndex = aLineNumber ]