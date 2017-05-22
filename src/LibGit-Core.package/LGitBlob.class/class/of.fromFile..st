instance creation
of: aRepository fromFile: aFileReference
	| id |
	id := LGitId new.
	self blob_create_fromdisk: id repo: aRepository path: aFileReference fullName.
	^ (self of: aRepository)
		initializeWithId: id;
		yourself