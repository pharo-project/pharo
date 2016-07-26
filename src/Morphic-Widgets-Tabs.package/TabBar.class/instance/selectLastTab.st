protocol
selectLastTab
	| tab |
	
	tab := tabs lastIfAbsent: [ ^ self ].
	tab selected: true