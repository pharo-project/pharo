rendering
renderOn: aTextRenderer
	
	commands ifNil: [ ^ self ]. 
	commands do: [ :cmd |
		cmd renderOn: aTextRenderer
	].
