initialize
computeWeight: valuable
	"Set the way to compute the weight of each cached value.
	This can be either a Symbol or one argument block.
	When the total weight is no longer below the maximum weight,
	the least recently used key/value pair will be removed.
	The default way to compute the weight returns 1 for each value,
	effectively counting the number of cached values."
	
	weight compute: valuable