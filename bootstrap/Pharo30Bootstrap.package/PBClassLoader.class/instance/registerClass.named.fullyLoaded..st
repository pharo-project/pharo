registration
registerClass: aClassMirror named: aName fullyLoaded: aBoolean

	"If it existed before, we migrate its state"
	(environment includesKey: aName)
		ifTrue: [ 
			self migrateOldClass: (environment at: aName) to: aClassMirror ].

	"We register it and keep up to date our compact class index"
	environment at: aName put: aClassMirror.
	aClassMirror asClassMirror compactClassIndex = 0 ifFalse: [ 
		builder objectSpace backend compactClassAt: aClassMirror asClassMirror compactClassIndex put: aClassMirror ].
	
	"We create a mapping between the real classes and their definitions for later"
	self map: aClassMirror with: (systemDefinition classNamed: aName).
	
	"We reset the class map of the ast interpreter"
	fullyLoaded at: aName put: aBoolean