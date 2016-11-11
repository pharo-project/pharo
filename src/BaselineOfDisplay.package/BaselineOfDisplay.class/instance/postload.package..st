actions
postload: loader package: packageSpec

	| display |
	
	self installBitmap.

	display := (Smalltalk classNamed: #DisplayScreen) new.
	display setExtent: (Smalltalk classNamed: #DisplayScreen) actualScreenSize depth: 2.
	Smalltalk globals at: #Display put: display.
	display beDisplay.

	Cursor classPool at: #CurrentCursor put: Cursor new.
	Cursor classPool at: #WebLinkCursor put: Cursor new.
					
	Cursor initialize.
	DisplayScreen initialize.
	BitBlt initialize.
	Color initialize.
	
	MCMethodDefinition initializersEnabled: initializersEnabled.
	
	