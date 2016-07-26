as yet unclassified
logPartialMatch: anEvent

	debug == true ifFalse: [ ^self ].
	
	self logger 
		nextPutAll: 'Partial match: ';
		nextPutAll: anEvent printString;
		cr;
		flush.