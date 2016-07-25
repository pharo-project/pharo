private 
isStale: association
	"Delegate the decision to the association itself 
	with our time to live as default"
	
	^ association isStale: timeToLive