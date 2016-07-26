instance creation
createSharedPoolNamed: aSymbol
	"A shared pool is a class inheriting from #SharedPool"

	^(RGClassDefinition named: aSymbol)
			superclassName: #SharedPool;
			isPool: true;
			yourself