keyboard control
filterListWith: char 
	| matchString |
	matchString := self valueOfProperty: #matchString ifAbsentPut: [String new].
	matchString := char = Character backspace 
				ifTrue: 
					[matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: char].
	self setProperty: #matchString toValue: matchString