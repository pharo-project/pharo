instance creation
owner: aMenuItem keyText: aString
	^ self shortcutClass basicNew 
		initializeOwner: aMenuItem keyText: aString;
		yourself