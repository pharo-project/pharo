converting
pathFromString: aString
	"Use the unix convention by default, since many filesystems are based on it."
	
	^ Path from: aString delimiter: self delimiter