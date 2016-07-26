shortcut creation
remind: aMenuItem 

	| itemHash |
	
	itemHash := (aMenuItem contents hash + aMenuItem keyText hash) hashMultiply.
	
	(self wantsToShow: itemHash) ifFalse: [ ^self ].
	
	self 
	show: aMenuItem;
	updateCount: itemHash.