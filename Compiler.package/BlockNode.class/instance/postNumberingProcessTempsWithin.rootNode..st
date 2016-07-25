code generation (closures)
postNumberingProcessTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>"
	"A temp can be local (and copied) if it is not written to after it is captured.
	 A temp cannot be local if it is written to remotely.
	 Need to enumerate a copy of the temporaries because any temps becoming remote
	 will be removed from temporaries in analyseClosure: (and a single remote temp node
	 will get added)"
	temporaries copy do:
		[:each|
		each isIndirectTempVector ifFalse:
			[each analyseClosure: rootNode]].

	"If this is an optimized node we need to hoist temporaries up into the relevant block scope."
	optimized ifTrue:
		[self optimizedBlockHoistTempsInto: scopeBlock].

	"Now we may have added a remoteTempNode.  So we need a statement to initialize it."
	self ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode.

	"Now add all arguments and locals to the pool so that copiedValues can be computed during sizing."
	rootNode
		addLocalsToPool: arguments;
		addLocalsToPool: temporaries