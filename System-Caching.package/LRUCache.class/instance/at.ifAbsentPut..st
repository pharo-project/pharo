accessing
at: key ifAbsentPut: block
	"If key is present in the cache, return the associated value.
	This is a hit and makes that key/value pair the most recently used.
	If key is absent, use block to compute a new value and cache it.
	Block can optionally take one argument, the key. 
	This is a miss and will create a new key/value pair entry.
	Furthermore this could result in the least recently used key/value pair 
	being removed when the specified maximum cache weight is exceeded."

	self critical: [ | association |
		association := keyIndex
			associationAt: key
			ifAbsent: [ | value |
				value := block cull: key.
				"Sadly we have to check the presence of key again 
				in case of the block execution already added the entry"
				keyIndex 
					associationAt: key 
					ifAbsent: [
						association := self newAssociationKey: key value: value.
						^ self handleMiss: association ] ].
		^ self handleHit: association ]