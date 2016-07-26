printing
print24: hr24 on: aStream 
 	"Format is 'hh:mm:ss' or 'h:mm:ss am' "
 
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
	aStream nextPutAll: (s < 10 ifTrue: [ ':0' ] ifFalse: [ ':' ]).
	s printOn: aStream.
	hr24 ifFalse: [ aStream nextPutAll: (h < 12 ifTrue: [ ' am' ] ifFalse: [ ' pm' ]) ]