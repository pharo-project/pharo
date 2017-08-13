private
isWhitespaceCharacter: character
	"Return true when character should be considered whitespace"

	whitespace 
		ifNil: [ "No whitespace allowed" 
			^ false ].
	whitespace = #separator
		ifTrue: [ "Only separators are considered whitespace" 
			^ character isSeparator ].
	whitespace = #any
		ifTrue: [ "All non-legal (non-alphabet) characters are considered whitespace" 
			^ (self isLegalCharacter: character) not ].
	^ false