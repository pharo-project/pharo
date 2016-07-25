actions
printVersion
	self stdout 
		nextPutAll: '[version] ';
		print: SystemVersion current major;
		nextPutAll: '.';
		print: SystemVersion current minor;
		nextPutAll: ' #' ;
		print: SystemVersion current highestUpdate;
		lf.