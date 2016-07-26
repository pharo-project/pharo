actions
printRelease
	self stdout 
		print: SystemVersion current major;
		nextPutAll: '.';
		print: SystemVersion current minor;
		lf.