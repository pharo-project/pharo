private
consumeToEOL
	| eol char |
	eol := false.
	[ eol ] whileFalse: [ 
		char := stream next.
		(char isNil or: [ char = Character lf ])
			ifTrue: [ eol := true ]
			ifFalse: [ 
				char = Character cr
					ifTrue: [ 
						eol := true.
						stream peekFor: Character lf ] ] ]