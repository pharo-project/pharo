comparing
= anObject
	"This is needed to handle the update of the whole list while still preserving the selection"
	^ self species = anObject species and: [ self item = anObject item ]