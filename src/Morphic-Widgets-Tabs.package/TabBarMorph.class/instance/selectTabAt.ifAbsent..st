protocol
selectTabAt: index ifAbsent: aBlock
	| tab |
	
	tab := tabs at: index ifAbsent: [ ^ aBlock value ].
	tab selected: true