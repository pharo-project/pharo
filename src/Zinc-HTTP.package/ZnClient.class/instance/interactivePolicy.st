initialization
interactivePolicy
	self 
		beOneShot;
		ifFail: [ :exception | 
			self inform: 'HTTP call failed with ', exception printString.
			nil ]