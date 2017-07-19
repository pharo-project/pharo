instruction stream support
isPushTempAt: pc in: method
	"64-71		01000 iii			Push Temp #iii
	 72-75		010010 ii			Push Temp #ii + 8
	 229		11100101	iiiiiiii	Push Temporary Variable #iiiiiiii"

	| byte |
	((byte := method at: pc) between: 64 and: 75) ifTrue: [ ^ true ].
	^ byte = 229