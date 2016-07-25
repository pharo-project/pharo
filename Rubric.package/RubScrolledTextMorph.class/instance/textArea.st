accessing
textArea
	^ self scrollPane ifNotNil: [:sp | sp textArea]