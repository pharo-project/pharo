accessing
setConnectionCloseFor: request
	request wantsConnectionClose 
		ifTrue: [ self setConnectionClose ]