read, write, position
flush
	"Flush the external OS stream (the one in the C library)."
	
	self systemAccessor fflush: self getFilePointerAsCFile