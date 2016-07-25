accessing request
resetEntity
	"Make sure that the current request does not hold an entity from a previous request.
	This is sometimes necessary after a POST or PUT. This method will be called automatically
	by #method: for certain requests."
	
	(request notNil and: [ request hasEntity ])
		ifTrue: [ request resetEntity: nil ]