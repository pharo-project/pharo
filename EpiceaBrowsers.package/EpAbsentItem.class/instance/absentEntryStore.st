private
absentEntryStore

	^ (OmFileStore 
		named: entryReference globalName
		inDirectory: OmSessionStore defaultBaseLocator asFileReference)
		refresh;
		yourself