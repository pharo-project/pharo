removing
removeStaleValues
	"Go over all cached values and remove those that are stale.
	Remove a collection of the keys that were removed."

	^ self critical: [
		| keysToRemove |
		keysToRemove := OrderedCollection new.
		lruList do: [ :association |
			 (self isStale: association) 
				ifTrue: [ keysToRemove add: association key ] ].
		keysToRemove do: [ :each | self removeKey: each ].
		keysToRemove ]