parsing
skipSpace
	| ch |

	[ 
		ch := stream peek.
		ch ifNil: [ ^ self ].
		ch isSeparator ] 
	whileTrue: [ stream next ]