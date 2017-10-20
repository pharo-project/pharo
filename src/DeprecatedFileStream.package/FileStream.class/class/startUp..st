system startup
startUp: isImageStarting
	isImageStarting
		ifFalse: [ ^ self ].
	self voidStdioFiles.
	[ TheStdioHandles := self stdioHandles ]
		on: Error
		do: [ :ex | 
			TheStdioHandles isArray
				ifFalse: [ TheStdioHandles := Array new: 3 ] ]