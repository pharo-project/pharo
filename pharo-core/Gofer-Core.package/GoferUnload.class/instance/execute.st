running
execute
	self workingCopies 
		do: [ :copy | self unload: copy ].
	self model load.
	self gofer cleanup.
	self workingCopies 
		do: [ :copy | self unregister: copy ]