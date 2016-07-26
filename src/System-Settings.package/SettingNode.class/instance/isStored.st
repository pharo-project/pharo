testing
isStored
	"Return true if this setting node has stored value. 
	The stored value can differ from the current value."
	^ self storedValue notNil