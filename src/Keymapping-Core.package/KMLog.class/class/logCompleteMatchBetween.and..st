as yet unclassified
logCompleteMatchBetween: aMorph and: aKeymap

	debug == true ifFalse: [ ^self ].
	
	self logger 
		nextPutAll: aMorph printString;
		nextPutAll: ' Complete match: ';
		nextPutAll: aKeymap printString;
		cr;
		flush.