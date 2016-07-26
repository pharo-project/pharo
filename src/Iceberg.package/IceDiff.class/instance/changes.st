querying
changes
	^ changes ifNil: [changes := 
		repository loadedPackages flatCollect: [ :package | | mine theirs |
			mine := self mySnapshot: package.
			theirs := self theirSnapshot: package.
			(mine notNil & theirs notNil) 
				ifTrue: [ (MCPatch fromBase: theirs target: mine) operations ]
				ifFalse: #()
	 ]]
