accessing request
contents: object
	"Set the entity contents of the current request to object.
	I will pass this through my contentWriter if I have one.
	Otherwise I delegate to ZnEntity to convert object to a generic entity."
	
	self contentWriter 
		ifNil: [ self entity: (ZnEntity with: object) ]
		ifNotNil: [ :writer | self entity: (writer value: object) ]