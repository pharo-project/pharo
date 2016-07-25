running
execute
	self workingCopies
		do: [ :each | self cleanup: each ]