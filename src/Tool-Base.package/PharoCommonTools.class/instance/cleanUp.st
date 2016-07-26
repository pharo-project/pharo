cleanup
cleanUp
	recentTools keysAndValuesDo:[:name :toolSet|
		toolSet copy do:[:tool | tool isObsolete ifTrue:[ toolSet remove: tool]]]