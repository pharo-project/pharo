*GT-Debugger
asGlamourAction

	^ ((GLMGenericAction new) 
			action: [ self execute ]; 
			icon: self icon;
			title: self label;
			showTitle;
			shortcut: (self keyText ifNotNil: [:aString | aString at: 1]);
			category: self category;
			yourself)