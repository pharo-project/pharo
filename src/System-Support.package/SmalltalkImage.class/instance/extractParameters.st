system attribute
extractParameters

	| pName value index dict |
	dict := Dictionary new.
	index := 3. "Muss bei 3 starten, da 2 documentName ist"
	[pName := self vm getSystemAttribute: index.
	pName isEmptyOrNil] whileFalse:[
		index := index + 1.
		value := self vm getSystemAttribute: index.
		value ifNil: [value := ''].
 		dict at: pName asUppercase put: value.
		index := index + 1].
	^dict