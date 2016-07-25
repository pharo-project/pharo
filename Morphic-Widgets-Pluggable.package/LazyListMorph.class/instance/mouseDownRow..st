accessing
mouseDownRow: anInteger
	"Set the row that should have mouse down highlighting or nil if none."

	anInteger = self mouseDownRow ifTrue: [^self].
	self mouseDownRowFrameChanged.
	self setProperty: #mouseDownRow toValue: anInteger.
	self mouseDownRowFrameChanged