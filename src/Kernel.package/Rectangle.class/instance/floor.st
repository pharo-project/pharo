truncation and roundoff
floor
	"Answer the integer rectangle to the topLeft of receiver.
	Return receiver if it is already an integerRectangle."

	self isIntegerRectangle ifTrue: [ ^ self ] .

	^ origin floor corner: corner floor