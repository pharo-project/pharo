private
validateInvariantWith: assertable
	"The keyIndex maps keys to double linked list nodes that hold 
	a key value association, whose key must match the index key"
	keyIndex keysAndValuesDo: [ :key :link |
		assertable assert: link value key = key ].
	"When iterating over all key value pairs, each key lookup up
	through the index should see the correct key and value in 
	the double linked list node"
	self keysAndValuesDo: [ :key :value |
		| link |
		link := keyIndex at: key.
		assertable assert: link value value = value.
		assertable assert: link value key = key ].
	"When iterating over all keys, each key can be present only once"
	Set new in: [ :keysSeen |
		self keysAndValuesDo: [ :key :value |
			assertable assert: (keysSeen includes: key) not.
			keysSeen add: key ] ]