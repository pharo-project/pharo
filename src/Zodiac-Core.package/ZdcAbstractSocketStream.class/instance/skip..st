stream in
skip: count
	"Skip over count bytes.
	This is an inefficient abstract implementation skipping bytes one by one."
	
	count timesRepeat: [ self next ]