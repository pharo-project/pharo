accessing
entryByPath: aString ifAbsent: aBlock
	| entry |
	entry := LGitTreeEntry of: self repository.
	self tree_entry_bypath: entry path: aString.
	^ entry isNull
		ifTrue: [ aBlock value ]
		ifFalse: [ 
			entry repository: self repository.
			entry ]