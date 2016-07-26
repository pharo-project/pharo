as yet unclassified
log: aKeyEvent

	debug == true ifFalse: [ ^self ].
	
	self logger
		nextPutAll: aKeyEvent printString;
		cr;
		flush.