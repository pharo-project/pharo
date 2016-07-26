removing
removeAll
	"Remove all key/value pairs that I currently hold,
	effectiley resetting me, but not my statistics."
	
	self critical: [
		lruList removeAll.
		keyIndex removeAll.
		weight reset ]