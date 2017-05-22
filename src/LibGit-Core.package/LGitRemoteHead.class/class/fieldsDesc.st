field definition
fieldsDesc
	"self rebuildFieldAccessors"
	^#(
	int local; " available locally "
	LGitId oid;
	LGitId loid;
	char *name;
	"
	 * If the server send a symref mapping for this ref, this will
	 * point to the target.
	"
	char *symref_target;
	)