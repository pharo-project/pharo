accessing
skip: count
	"Skip over count elements.
	This could be further optimzed."
	
	count timesRepeat: [ self next ]