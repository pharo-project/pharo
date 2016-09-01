system annoucements
classRemoved: aClass

	(rbEnvironment definesClass: aClass)
		ifTrue: [ cache itemRemoved: aClass ].
		
	cache updateBrowser