signaling
signal: anArray
	"Signal the occurrence of an exceptional condition with a specified cacheName."

	self cacheNames: anArray.
	^ self signal