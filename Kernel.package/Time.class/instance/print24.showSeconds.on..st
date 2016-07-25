printing
print24: hr24 showSeconds: showSeconds on: aStream 
	"Format is 'hh:mm:ss.nnnnnnnnn' or 'h:mm:ss.nnnnnnnnn am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	| h m s |
	h := self hour. m := self minute. s := self second.
	hr24
		ifTrue: [ 
			h < 10 ifTrue: [ aStream nextPut: $0 ].
			h printOn: aStream ]
		ifFalse: [ 
			h > 12
				ifTrue: [ h - 12 printOn: aStream ]
				ifFalse: [
					h < 1
						ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ] ] ].
	aStream nextPutAll: (m < 10 ifTrue: [ ':0' ] ifFalse: [ ':' ]).
	m printOn: aStream.
	showSeconds 
		ifTrue: [
			aStream nextPutAll: (s < 10 ifTrue: [ ':0' ] ifFalse: [ ':' ]).
			s printOn: aStream.
			nanos = 0
				ifFalse: [ | n len |
					n := nanos. len := 9.
					[ n \\ 10 = 0 ] whileTrue: [ n := n / 10. len := len - 1 ].
					aStream nextPut: $..
					n printOn: aStream base: 10 length: len padded: true ] ].
	hr24 ifFalse: [ aStream nextPutAll: (h < 12 ifTrue: [ ' am' ] ifFalse: [ ' pm' ]) ]