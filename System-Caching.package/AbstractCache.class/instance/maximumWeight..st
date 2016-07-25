initialize
maximumWeight: limit
	"Set my maximum allowed total weight of all cached values to limit.
	If the total weight is no longer below limit,
	the least recently used key/value pair will be removed.
	The default maximum weight limit is 16."
	
	weight maximum: limit