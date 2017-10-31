private
nextCounter16
	"Return the next 16-bit counter value with safe overflow"
	
	counter := (counter + 1) \\ 16r10000.
	^ counter