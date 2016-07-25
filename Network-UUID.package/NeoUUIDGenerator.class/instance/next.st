accessing
next
	"Return the next UUID, see my class comment"
	
	| uuid |
	uuid := UUID nilUUID.
	self placeFields: uuid.
	^ uuid