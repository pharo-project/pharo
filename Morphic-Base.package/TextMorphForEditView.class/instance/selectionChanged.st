private
selectionChanged
	self sharesFindReplace 
		ifTrue: [self editor findReplaceService changed: #findReplaceSelection].
	self useExtraSelection
		ifTrue: [self refreshExtraSelection].
	super selectionChanged