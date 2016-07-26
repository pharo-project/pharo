keymaps
attach: aCategory to: aClass
	(self categoryHolders includesKey: aClass) 
		ifFalse: [ self categoryHolders at: aClass put: Set new ].
	
	(self categoryHolders at: aClass) add: aCategory