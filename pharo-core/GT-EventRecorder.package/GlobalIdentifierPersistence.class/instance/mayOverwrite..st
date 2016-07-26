load and saving
mayOverwrite: aDictionary
	"If I keep previous persistence strategy, I am the main one and I can overwrite the current preference file.
	I am called only on loading failure; it means when I cannot load preference file."
	self hasPreviousPersistence ifTrue: [ self save: aDictionary ].