printing
printKeyStringOn: aStream
	"Print a readable string representing the receiver on a given stream"

	| kc inBrackets firstBracket keyString |
	kc := self keyCharacter.
	inBrackets := false.
	firstBracket := [ inBrackets ifFalse: [ aStream nextPut: $<. inBrackets := true ]].
	self controlKeyPressed ifTrue: [ 	firstBracket value. aStream nextPutAll: 'Ctrl-' ].
	self commandKeyPressed ifTrue: [ firstBracket value. aStream nextPutAll: 'Cmd-' ].
	(buttons anyMask: 32) ifTrue: [ firstBracket value. aStream nextPutAll: 'Opt-' ].
	(self shiftPressed and: [ keyValue between: 1 and: 31 ])
		ifTrue: [ firstBracket value. aStream nextPutAll: 'Shift-' ].

	self hasSpecialCTRLKeyValue
			ifTrue:
				[aStream nextPut: (keyValue + $a asciiValue - 1) asCharacter]
			ifFalse: 
				[keyString := (kc caseOf: {
					[ Character space ] -> [ ' ' ].
					[ Character tab ] -> [ 'tab' ].
					[ Character cr ] -> [ 'cr' ].
					[ Character lf ] -> [ 'lf' ].
					[ Character enter ] -> [ 'enter' ].

					[ Character backspace ] -> [ 'backspace' ].
					[ Character delete ] -> [ 'delete' ].

					[ Character escape ] -> [ 'escape' ].

					[ Character arrowDown ] -> [ 'down' ].
					[ Character arrowUp ] -> [ 'up' ].
					[ Character arrowLeft ] -> [ 'left' ].
					[ Character arrowRight ] -> [ 'right' ].

					[ Character end ] -> [ 'end' ].
					[ Character home ] -> [ 'home' ].
					[ Character pageDown ] -> [ 'pageDown' ].
					[ Character pageUp ] -> [ 'pageUp' ].

					[ Character euro ] -> [ 'euro' ].
					[ Character insert ] -> [ 'insert' ].

				} otherwise: [ String with: kc ]).
				keyString size > 1 ifTrue: [ firstBracket value ].
				aStream nextPutAll: keyString].

	inBrackets ifTrue: [aStream nextPut: $> ]