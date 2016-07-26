code generation (closures)
ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode
	"If a remoteTempNode has been added ensure a statement exists to initialize it."
	remoteTempNode ~~ nil ifTrue:
		[(statements notEmpty
		  and: [statements first isAssignmentNode
		  and: [statements first variable isTemp
		  and: [statements first variable isIndirectTempVector]]])
			ifTrue: "If this is a decompiled tree, or if a temporary has been added later in
					the analysis then there already is a temp vector initialization node."
				[(statements first variable ~~ remoteTempNode) ifTrue:
					[statements first variable become: remoteTempNode].
				 statements first value numElements: remoteTempNode remoteTemps size]
			ifFalse:
				[statements addFirst: (remoteTempNode nodeToInitialize: rootNode encoder)]].