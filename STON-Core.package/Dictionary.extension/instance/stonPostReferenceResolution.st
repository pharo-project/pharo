*ston-core
stonPostReferenceResolution
	"When references were resolved in me, the hash of my keys might have changed.
	Check if I am still healthy and rehash me if not."
	
	self isHealthy ifFalse: [ self rehash ]