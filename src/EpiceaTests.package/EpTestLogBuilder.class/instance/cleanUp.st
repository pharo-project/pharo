releasing
cleanUp
	"Remove log file and directory, if empty."
	
	| reference |
	reference := log store fileReference.
	reference ensureDelete.
	reference parent hasChildren ifFalse: [
		reference parent ensureDelete ].
	log := nil.