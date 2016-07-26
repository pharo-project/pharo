truncation and roundoff
ceiling
	"Answer the integer rectangle to the bottom right of receiver.
	Return receiver if it is already an integerRectangle."

	self isIntegerRectangle ifTrue: [ ^ self ].
	^origin ceiling corner: corner ceiling