parsing
parseWord
	^ String streamContents: [:st | | ch |
		"first char must be letter or underscore"
		ch := stream peek.	
		(ch notNil and: [ ch isLetter or: [ '_$' includes: ch ]]) ifFalse: [ ^ nil ].
		[ 
			ch := stream peek. 
			ch notNil 
				and: [ 
					ch isLetter 
					or: [ ('_$' includes: ch)  
					or: [ch isDigit] ] ] ] 
		whileTrue: [ st nextPut: stream next ] ]

