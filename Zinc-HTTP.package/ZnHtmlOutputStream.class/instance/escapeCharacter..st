html
escapeCharacter: char
	"Write char, escaping it as needed for regular text"

	char == $<
		ifTrue: [ stream nextPutAll: '&lt;' ]
		ifFalse: [ 
			char == $&
				ifTrue: [ stream nextPutAll: '&amp;' ]
				ifFalse: [ stream nextPut: char ] ]