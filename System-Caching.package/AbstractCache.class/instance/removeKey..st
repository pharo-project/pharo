removing
removeKey: key
	"If I currently cache key, remove the entry.
	Signal a KeyNotFound when I currently do not cache key.
	Return the removed value."
	
	^ self 
		removeKey: key 
		ifAbsent: [ KeyNotFound signalFor: key in: self ]