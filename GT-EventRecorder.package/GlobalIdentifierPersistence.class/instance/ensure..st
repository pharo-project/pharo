load and saving
ensure: existingDictionary
	"It proceeds all backward compatibility work."
	self shouldCallPreviousPersistence ifTrue: [ 
		previousPersistence ensure: existingDictionary ].
	self load: existingDictionary