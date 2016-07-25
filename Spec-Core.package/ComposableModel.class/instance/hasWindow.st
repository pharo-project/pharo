testing
hasWindow
	"Answers true if there is an open window using this model."

	self owner ifNil: [ ^ self window ifNotNil: [ :w | w isClosed not ] ifNil: [ false ] ].
	 ^ self owner hasWindow