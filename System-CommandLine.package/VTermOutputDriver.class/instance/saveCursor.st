navigating
saveCursor
	outStream nextPutAll: (termcap getstr: 'sc')