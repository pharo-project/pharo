iterating
sendCommandsTo: aBuilder

	self do: [ :segment |
		segment sendCommandTo: aBuilder
	]