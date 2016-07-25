accessing
peek
	"We do byte/char conversions on the fly."
	
	^ self atEnd 
		ifTrue: [ nil ] 
		ifFalse: [ 
			binary isNil
				ifTrue: [ stream peek ] 
				ifFalse: [ 
					binary
					 	ifTrue: [ stream peek asInteger ]
						ifFalse: [ stream peek asCharacter ] ] ]