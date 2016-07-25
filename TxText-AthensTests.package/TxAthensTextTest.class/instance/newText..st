private
newText: aString
	| text |
	text := TxModel new.
	text endPosition insert: aString.
	^ text