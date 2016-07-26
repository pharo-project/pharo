initialization
encodeTitle: aTitle
	| allowedCharacters |
	
	allowedCharacters := ($A to: $Z)
				, ($a to: $z)
				, ($0 to: $9)
				, (Array with: $-).
	^ String streamContents: [ :stream | 			
		aTitle
			do: [ :character | (' 	_' includes: character)
				ifTrue: [ stream << '-' ]
				ifFalse: [ (allowedCharacters includes: character)
						ifTrue: [ stream << character ] ] ] ]