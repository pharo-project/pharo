operation
at: aTxTextPosition insertSingleLine: aString 

	prev isCharacterSpan ifFalse: [
		self insertPrev: text newCharacterSpan 
		].
	
	^ prev at: prev endPosition insertSingleLine: aString 