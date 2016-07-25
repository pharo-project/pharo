accessing
allHandlers
	^ self allSubclasses reject: [ :handler| handler isAbstract ]