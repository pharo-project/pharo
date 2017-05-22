operations
writeTreeAndReturnOid
	| oid |
	oid := LGitId externalNew.
	self withReturnHandlerDo: [ 
		self
			index_write_tree: oid
			index: self ].
		
	^ oid