scanning
nextTabXFrom: anX
	"Tab stops are distances from the left margin. Set the distance into the 
	argument, anX, normalized for the paragraph's left margin."

	| normalizedX tabX mx |
	mx := 9999.
	normalizedX := anX - leftMargin.
	tabX := self tabWidth.
	[ tabX <= normalizedX and: [ tabX < mx ] ] whileTrue: [ tabX := tabX + self tabWidth ].
	^ tabX < mx
		ifTrue: [ leftMargin + tabX min: rightMargin ]
		ifFalse: [ rightMargin ] 