ui requests
chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString
	 ^ self nonInteractiveWarning: (String streamContents: [ :s|
			s nextPutAll: 'Choice Request: '; nextPutAll: aString; cr.
			labelList with: valueList do: [ :label :value| 
				s tab; nextPutAll: label asString; nextPutAll: ':'; tab; print: value; cr ]])