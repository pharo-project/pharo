running
execute
	self workingCopies
		do: [ :each | self execute: each ]