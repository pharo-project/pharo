accessing
headReference
	"Answer a reference to the last entry added to this store, or OmNullReference uniqueInstance when empty."

	^ entries
		ifEmpty: [ OmNullReference uniqueInstance ]
		ifNotEmpty: [ self referenceTo: entries last ]