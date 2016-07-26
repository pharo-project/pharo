initialization
registerForChanges
	methodHolder
		whenChangedDo:
			[ :contents | 
			| boolean |
			boolean := contents ifNil: [ false ] ifNotNil: [ true ].
			browseModel state: boolean.
			usersModel state: boolean.
			sendersModel state: boolean.
			versionModel state: boolean.
			implementorsModel state: boolean ]