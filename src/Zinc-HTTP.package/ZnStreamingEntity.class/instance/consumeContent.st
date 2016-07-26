accessing
consumeContent
	"To be improved later on using some kind of larger buffer"
	
	[ self stream atEnd ] 
		whileFalse: [ self stream next ]