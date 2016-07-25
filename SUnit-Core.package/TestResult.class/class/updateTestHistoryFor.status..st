history
updateTestHistoryFor: aTestCase status: aSymbol
	| cls sel |
	
	cls := aTestCase class.
	sel := aTestCase selector.
	self removeFromTestHistory: sel in: cls.
	((self historyAt: cls) at: aSymbol ) add: sel.