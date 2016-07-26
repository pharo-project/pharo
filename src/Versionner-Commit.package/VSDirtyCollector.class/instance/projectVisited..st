testing
projectVisited: aProject
	^ projects anySatisfy: [ :each | each configurationClass =  aProject configurationClass ]