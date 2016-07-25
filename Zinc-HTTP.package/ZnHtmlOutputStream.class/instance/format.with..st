html
format: string with: args
	"Like String>>#format: format the string template using the args given, esacaping characters when needed for regular text"

	| input currentChar |
	input := string readStream.
	[ input atEnd ] whileFalse: [
		(currentChar := input next) == ${
			ifTrue: [ | expression index | 
				expression := input upTo: $}.
				index := Integer readFrom: expression ifFail: [ expression ].
				self escape: (args at: index) asString ]
			ifFalse: [
				currentChar == $\
					ifTrue: [ input atEnd ifFalse: [ self escapeCharacter: stream next ] ]
					ifFalse: [ self escapeCharacter: currentChar ] ] ]