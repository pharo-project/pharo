My job is to scan bytecodes for instance variable references.

BlockContext allInstances collect: [ :x |
	{x. x hasInstVarRef}
].