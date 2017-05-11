accessing
entryReferences
	"Answer OmReference instances for all the entries in this store"

	^ self entries collect: [:each | self referenceTo: each ]