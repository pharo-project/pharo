build ui lists
buildNavigationList
	| navigation |
	navigation := (DropListMorph
		on: self
		list: #getHistoryList
		selected: #currentHistoryIndex 
		changeSelected: #setHistory:)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		wrapSelector: #wrapHistory:;
		ghostText: 'History Navigator';
		yourself.
	self setShortcuts: #NautilusSourceCodeShortcuts to: navigation.
	^ navigation