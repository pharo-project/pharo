accessing
project
	^ (self projectActions 
		detect: [ :each | each project configurationClass = self configurationClass ]
		ifNone: [ ^ nil ])
		project
		