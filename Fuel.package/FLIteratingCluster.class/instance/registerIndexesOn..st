serialize/materialize
registerIndexesOn: anEncoder
	| indexes |
	indexes := anEncoder objectsIndexes.
	self objects do: [ :instance |
		indexes
			at: instance
			put: indexes size + 1 ]