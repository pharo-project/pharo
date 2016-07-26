accessing
errorWhileAccessing: selector do: aBlock
	[ self perform: selector ]
		on: Error do: [ :error |  
			aBlock cull: error cull: selector ].