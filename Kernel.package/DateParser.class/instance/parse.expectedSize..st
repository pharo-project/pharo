parsing - private
parse: timeUnitName expectedSize: anInteger
	| extractedString result |
	extractedString := inputStream next: anInteger.
	result := extractedString asInteger.
	(result isNil or: [ extractedString size ~= anInteger ]) 
		ifTrue: [ DateError signal: ' Expect a two digit ', timeUnitName, ', got ', extractedString ].
	^ result