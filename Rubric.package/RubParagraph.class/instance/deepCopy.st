copying
deepCopy
	"Don't want to copy the container (etc) or fonts in the TextStyle."

	^ self error: 'A ParagraphHandler should never be deep copied'.