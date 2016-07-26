testing
isDescribingEntity
	"Do I include enough information to describe an entity (i.e. content length and type) ?"
	
	^ (self headers includesKey: 'Content-Type') 
		and: [ self headers includesKey: 'Content-Length' ]
