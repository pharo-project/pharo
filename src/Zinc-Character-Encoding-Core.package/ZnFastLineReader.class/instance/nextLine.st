accessing
nextLine
	"Read a CR, LF or CRLF terminated line, returning the contents of the line without the EOL. Return nil when the receiver is #atEnd."
	
	self atEnd ifTrue: [ ^ nil ].
	^ self streamContents: [ :out | | eol char |
		eol := false.
		[ eol ] whileFalse: [ 
			char := readStream next.
			(char isNil or: [ char == lf ])
				ifTrue: [ eol := true ]
				ifFalse: [ 
					char == cr 
						ifTrue: [ eol := true. readStream peekFor: lf ]
						ifFalse: [  out nextPut: char ] ] ] ]