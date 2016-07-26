access
timeStamp
	"Answer a TimeStamp that corresponds to my (text) stamp"

	| tokens |
	tokens := self stamp findTokens: Character separators.
	^ tokens size > 2
		ifTrue: [ 
			[ 
			| time date |
			date := Date fromString: (tokens at: tokens size - 1).
			time := Time fromString: tokens last.
			DateAndTime date: date time: time ]
				on: Error
				do: [ :ex | ex return: DateAndTime new ] ]
		ifFalse: [ DateAndTime new ]