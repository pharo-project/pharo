api
categories: aCollectionOfSymbols
	| cats |
	
	cats := aCollectionOfSymbols collect: [:e | KMRepository default categoryForName: e].
	categories value: cats.
	self updateShortcutList