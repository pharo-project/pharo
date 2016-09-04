accessing
next: n
	"Return a string with the next n characters of the filestream in it."
	^ self nextInto: (self collectionSpecies new: n)