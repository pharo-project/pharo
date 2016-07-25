initialization
checkForUnprintableCharacters: aString
	| types printables |
	types := String typeTable.
	printables := '!@#$%&*-_=+<>{}?/\,¬∑¬£¬¢¬ß¬∂¬™¬∫¬ñ¬ó¬ì¬ë¬î¬í¬Ö√ö√¶√ö¬Ø√ó¬ø¬´¬ª`~`' asSet.
	^ aString
		detect: [ :aChar | (types at: aChar asciiValue ifAbsent: [ #xLetter ]) == #xBinary and: [ (printables includes: aChar) not ] ]
		ifFound: [ :badChar | 
			'<<<This string contains a character (ascii value ' , badChar asciiValue printString
				, ') that is not normally used in code>>> ' , aString ]
		ifNone: [ aString ]