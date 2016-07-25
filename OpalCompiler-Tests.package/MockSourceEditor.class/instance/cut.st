as yet unclassified
cut

	text := (text first: selectionStart-1), (text copyFrom: selectionEnd+1 to: text size).
		
	selectionStart := selectionStart - 1.	
	selectionEnd := selectionStart