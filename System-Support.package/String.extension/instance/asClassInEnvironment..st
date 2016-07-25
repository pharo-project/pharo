*System-Support
asClassInEnvironment: anEnvironment
	"I return class named after myself from anEnvironment"
	^ self 
		asClassInEnvironment: anEnvironment 
		ifAbsent: [ NotFound signalFor: self in: anEnvironment  ]