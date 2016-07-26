accessing response
entity
	"Answer the entity or HTTP payload of the last response, 
	assuming the current request was executed."
	
	^ response ifNotNil: [ response entity ]  