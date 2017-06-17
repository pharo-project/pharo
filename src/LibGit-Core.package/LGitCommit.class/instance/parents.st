accessing
parents
	| parentCount |
	parentCount := self commit_parentcount: self.
	^ Array
		new: parentCount
		streamContents: [ :stream | 
			0 to: parentCount - 1 do: [ :i | 
				| parentOid parent |
				parentOid := LGitId fromHandle: (self commit_parent_id: self n: i) getHandle.
				parent := self class of: self repository fromId: parentOid.
				stream nextPut: parent ] ]