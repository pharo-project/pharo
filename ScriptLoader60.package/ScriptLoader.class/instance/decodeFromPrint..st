ci
decodeFromPrint: aString 
	^ aString 
		copyReplaceAll: $' asString, $' asString
		with: $' asString