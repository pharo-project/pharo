adding
addSeconds: nSeconds 
	"Answer a Time that is nSeconds after the receiver."

	^ self class seconds: self asSeconds + nSeconds