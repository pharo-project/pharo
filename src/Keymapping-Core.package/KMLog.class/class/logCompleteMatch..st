as yet unclassified
logCompleteMatch: aKeymap

	debug == true ifFalse: [ ^self ].
	
	self logger 
		nextPutAll: 'Complete match: ';
		nextPutAll: aKeymap printString;
		cr;
		flush.