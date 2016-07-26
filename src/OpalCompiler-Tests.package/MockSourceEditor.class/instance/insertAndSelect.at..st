as yet unclassified
insertAndSelect: aString at: anInteger

	text := (text first: anInteger-1), 
		(' ' , aString) asText, 
		(text copyFrom: anInteger to: text size).
		
	selectionStart := anInteger.
	selectionEnd := anInteger + aString size 