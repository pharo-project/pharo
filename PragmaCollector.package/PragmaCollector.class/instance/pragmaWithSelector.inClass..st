querying
pragmaWithSelector: aSelector inClass: aClass
	"return the handler corresponding to a pragma method which selector is aSelector in class aClass"

	^ self
		detect: [ :prag | prag methodClass = aClass and: [ prag methodSelector = aSelector ] ]