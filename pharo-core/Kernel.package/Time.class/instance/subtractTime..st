adding
subtractTime: timeAmount 
	"Answer a Time that is timeInterval before the receiver. timeInterval is  
	an instance of Date or Time."

	^ self class seconds: self asSeconds - timeAmount asSeconds