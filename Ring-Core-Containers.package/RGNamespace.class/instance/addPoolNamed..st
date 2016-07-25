adding elements
addPoolNamed: poolName
	"creates a RGPoolDefinition with the given name.
	It's verified that a pool with that name does not exist"
	| pool |

	(self includesPoolNamed: poolName) ifTrue: [ ^self ].
	"A shared pool is a class inheriting from #SharedPool"
	pool := (RGClassDefinition named: poolName)
			superclassName: #SharedPool;
			isPool: true;
			yourself.
	self addPool: pool.