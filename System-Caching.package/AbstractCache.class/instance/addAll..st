accessing
addAll: keyedCollection
	"Populate me with all key/value pairs from keyedCollection.
	Does not affect statistics."
	
	keyedCollection keysAndValuesDo: [ :key :value | 
		self at: key put: value ]