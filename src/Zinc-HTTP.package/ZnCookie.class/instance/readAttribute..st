initialize-release
readAttribute: aToken
	| i key data |
	i := aToken indexOf: $=.
	i > 0
		ifTrue: [
			key := (aToken copyFrom: 1 to: i - 1) trimBoth asLowercase.
			data := (aToken copyFrom: i + 1 to: aToken size) trimBoth ]
		ifFalse: [ 
			key := (aToken copyFrom: 1 to: aToken size) trimBoth asLowercase.
			data := true ].
	attributes at: key put: data