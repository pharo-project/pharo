accessing
at: key ifAbsentPut: block
	"Overwritten - see the comment of the method that I overwrite.
	In case of a hit, the cached value's timestamp is checked to see if its age
	does not exceed the allowed timeToLive. If so, the stale value is recomputed."
	
	self critical: [ | association |
		association := keyIndex 
			associationAt: key 
			ifAbsent: [ | value |
				value := block cull: key.
				"Sadly we have to check the presence of key again 
				in case of the block execution already added the entry"
				keyIndex 
					associationAt: key 
					ifAbsent: [ | newAssociation | 
						newAssociation := self newAssociationKey: key value: value.
						^ self handleMiss: newAssociation ] ].
		^ self handleHit: association ifStale: block ]