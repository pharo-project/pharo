operations
write
	| id errorCode |
	id := LGitId new.
	errorCode := self
		treebuilder_write: id
		bld: self.
		
	errorCode ~= 0 ifTrue: [ LGitWritingTreeFailed ].
	
	^ id