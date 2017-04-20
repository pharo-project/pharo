load and saving
load: existingDictionary
	"It loads stored information into existingDictionary."
	self isEnabled ifFalse: [ ^ self ].
	self preferences exists ifFalse: [
		"This is a new computer, so we define new computer UUID.
		User still has to agree about sending data if it is not has been done yet."
		^ self save: existingDictionary ].
	[ (self mergeExisting: existingDictionary stored: self load)
			ifTrue: [ self save: existingDictionary ].
	] on: Error do: [ 
		"Preferences likely contains a different settings version, so we store the actual one.
		We should keep the preferences as stable as possible."
		self mayOverwrite: existingDictionary ]