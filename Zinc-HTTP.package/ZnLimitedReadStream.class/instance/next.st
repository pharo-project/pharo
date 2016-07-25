accessing
next
	"We do byte/char conversions on the fly."
	
	^ self atEnd 
		ifTrue: [ nil ] 
		ifFalse: [ 
			position := position + 1. 
			binary isNil
				ifTrue: [ stream next ] 
				ifFalse: [ 
					binary
					 	ifTrue: [ stream next asInteger ]
						ifFalse: [ stream next asCharacter ] ] ]