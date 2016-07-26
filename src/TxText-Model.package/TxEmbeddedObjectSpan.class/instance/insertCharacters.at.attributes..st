operation
insertCharacters: aString at: aTxTextPosition attributes: aTxAttributes 
	^ self next insertCharacters: aString at: self next startPosition attributes: aTxAttributes 