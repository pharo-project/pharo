private
selectHandlers
	| handlers |
	handlers := (self subCommandsRoot selectHandlersFor: commandLine) copyWithout: self class.
	handlers := handlers sort: [ :a :b | a priority >= b priority ].
	^ handlers