*System-Hashing-Core
asInteger
	"Convert me to an Integer, network byte order, most significant byte first, big endian"
	
	| integer |
	integer := 0.
	self withIndexDo: [ :each :index | 
		integer := integer + (each bitShift: (self size - index) * 8) ].
	^ integer