handler selection
selectHandlersFor: aCommandLine

	^ self allHandlers select: [ :handlerClass|
		handlerClass isResponsibleFor: aCommandLine ]