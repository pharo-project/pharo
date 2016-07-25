removing
erase
	"outStream nextPutAll:
		(termcap parm: '1' in:
			(termcap getstr: 'ec'))"
	self shouldBeImplemented.