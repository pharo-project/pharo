accessing
version
	"Return a string of attributes representing the current VM"
	"
	Smalltalk vm version
	"
	^ String streamContents: [ :s |
			s nextPutAll: self interpreterClass; cr.
			self isRunningCogit ifTrue: [ s nextPutAll: self cogitClass; cr ].
			s nextPutAll: self interpreterSourceVersion; cr ].