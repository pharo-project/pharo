as yet unclassified
actOnClick: anEvent for: anObject in: paragraph editor: editor
	self notify: 'Please configure the actOnClickBlock to invoke the computation you need. In Pharo by default there is no external web browser'.
	"a possible implementation could be:
		 Smalltalk tools webBrowser openOnUrl: url."
