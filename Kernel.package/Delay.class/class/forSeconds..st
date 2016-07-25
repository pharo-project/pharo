instance creation
forSeconds: aNumber
	"Return a new Delay for the given number of Seconds"
	^ self forMilliseconds: aNumber * 1000
