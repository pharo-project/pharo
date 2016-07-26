display
showWaitCursorWhile: aBlock
	| process |
	process := [ [ true ] "loop until the end of aBlock processing"
		whileTrue: [ 
			Smalltalk logStdErrorDuring: [ :stream |
				"Wait wheel in ASCII art"
				'\|/-'
					do: [ :char |
						stream << char.
						0.1 second wait.
						stream << Character backspace ] ] ] ] fork.
	[ aBlock value ]
		ensure: [ process terminate ]