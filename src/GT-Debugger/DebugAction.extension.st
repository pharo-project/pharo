Extension { #name : #DebugAction }

{ #category : #'*GT-Debugger' }
DebugAction >> asGlamourAction [

	^ ((GLMGenericAction new) 
			action: [ self execute ]; 
			icon: self icon;
			title: self label;
			help: self help;
			showTitle;
			shortcut: (self keyText ifNotNil: [:aString | aString at: 1]);
			category: self category;
			yourself)
]
