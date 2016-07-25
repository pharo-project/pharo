running
execute
	self workingCopies
		do: [ :each | each modified: false ].
	super execute