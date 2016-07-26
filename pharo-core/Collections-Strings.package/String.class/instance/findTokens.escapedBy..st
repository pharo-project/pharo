accessing
findTokens: delimiters escapedBy: quoteDelimiters 
	"Answer a collection of Strings separated by the delimiters, where  
	delimiters is a Character or collection of characters. Two delimiters in a  
	row produce an empty string (compare this to #findTokens, which  
	treats sequential delimiters as one).  
	 
	The characters in quoteDelimiters are treated as quote characters, such  
	that any delimiter within a pair of matching quoteDelimiter characters  
	is treated literally, rather than as a delimiter.  
	 
	The quoteDelimiter characters may be escaped within a quoted string.  
	Two sequential quote characters within a quoted string are treated as  
	a single character.  
	 
	This method is useful for parsing comma separated variable strings for  
	spreadsheet import and export."
	| tokens rs activeEscapeCharacter ts char token delimiterChars quoteChars |
	delimiterChars := (delimiters 
		ifNil: [ '' ]
		ifNotNil: [ delimiters ]) asString.
	quoteChars := (quoteDelimiters 
		ifNil: [ '' ]
		ifNotNil: [ quoteDelimiters ]) asString.
	tokens := OrderedCollection new.
	rs := self readStream.
	activeEscapeCharacter := nil.
	ts := String new writeStream.
	[ rs atEnd ] whileFalse: 
		[ char := rs next.
		activeEscapeCharacter 
			ifNil: 
				[ (quoteChars includes: char) 
					ifTrue: [ activeEscapeCharacter := char ]
					ifFalse: 
						[ (delimiterChars includes: char) 
							ifTrue: 
								[ token := ts contents.
								tokens add: token.
								ts := String new writeStream ]
							ifFalse: [ ts nextPut: char ] ] ]
			ifNotNil: 
				[ char == activeEscapeCharacter 
					ifTrue: 
						[ rs peek == activeEscapeCharacter 
							ifTrue: [ ts nextPut: rs next ]
							ifFalse: [ activeEscapeCharacter := nil ] ]
					ifFalse: [ ts nextPut: char ] ] ].
	token := ts contents.
	(tokens isEmpty and: [ token isEmpty ]) ifFalse: [ tokens add: token ].
	^ tokens