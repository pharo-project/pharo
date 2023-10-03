Extension { #name : 'Float' }

{ #category : '*STON-Core' }
Float class >> fromSton: stonReader [
	| argument |
	argument := stonReader parseListSingleton.
	argument isNumber ifTrue: [ ^ argument ].
	^ (#(nan infinity negativeInfinity) includes: argument)
		ifTrue: [ self perform: argument ]
		ifFalse: [ stonReader error: 'special Float not recognized' ]
]
