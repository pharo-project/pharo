accessing
id
	| data |
	(id notNil or: [ self isNull ]) ifTrue: [ ^ id ].

	data := self get_id_data.
	id := LGitId fromHandle: data getHandle.
	id internalize.
	^ id	
