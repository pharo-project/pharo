accessing
at: key put: value
	"Populate me by storing value for key. Return value. 
	This is neither a hit nor a miss. Statistics remain unchanged.
	Overwrite if already present without promotion. 
	This could result in the least recently used key/value pair 
	being removed when the specified maximum cache weight is exceeded."
	
	self subclassResponsibility