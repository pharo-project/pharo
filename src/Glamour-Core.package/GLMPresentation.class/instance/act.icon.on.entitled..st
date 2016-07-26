scripting actions
act: aBlock icon: anIcon on: aCharacter entitled: aString
	self 
		addAction: 
			((GLMGenericAction new) 
				action: aBlock; 
				shortcut: aCharacter; 				
				icon: anIcon;
				title: aString;
				yourself)