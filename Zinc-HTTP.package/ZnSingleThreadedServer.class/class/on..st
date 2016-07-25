public
on: aNumber
	"Instantiate and return a new listener on a given port,
	send #start to it to start listening."

	^ self new 
		port: aNumber; 
		yourself