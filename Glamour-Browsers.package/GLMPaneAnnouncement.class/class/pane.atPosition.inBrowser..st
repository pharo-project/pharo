instance creation
pane: aPane atPosition: anInteger inBrowser: aBrowser 
	 
	^(self new) 
		pane: aPane; 
		position: anInteger; 
		browser: aBrowser; 
		yourself