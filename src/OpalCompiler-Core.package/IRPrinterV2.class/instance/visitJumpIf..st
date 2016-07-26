visiting
visitJumpIf: jumpIf

	stream nextPutAll: 'if: '.
	jumpIf boolean printOn: stream.
	stream nextPutAll: ' goto: '.
	jumpIf destination orderNumber printOn: stream.
	stream nextPutAll: ' else: '.
	jumpIf otherwise orderNumber printOn: stream.
