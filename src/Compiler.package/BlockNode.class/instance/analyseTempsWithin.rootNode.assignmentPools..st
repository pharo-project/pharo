code generation (closures)
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	| effectiveScope blockStart |
	effectiveScope := optimized
						ifTrue: [actualScopeIfOptimized := scopeBlock]
						ifFalse: [self].

	arguments ifNotNil:
		[arguments do: [:temp| temp definingScope: self]].
	temporaries ifNotNil:
		[temporaries do: [:temp| temp definingScope: self]].

	optimized ifFalse: "if optimized this isn't an actual scope"
		[rootNode noteBlockEntry:
			[:entryNumber|
			 blockExtent := (blockStart := entryNumber) to: 0]].

	"Need to enumerate a copy because closure analysis can add a statement
	 via ifHasRemoteTempNodeEnsureInitializationStatementExists:."
	statements copy do:
		[:statement|
		 statement analyseTempsWithin: effectiveScope rootNode: rootNode assignmentPools: assignmentPools].

	optimized
		ifTrue: "if optimized loop need to add nils for any temps read before written"
			[optimizedMessageNode isOptimizedLoop ifTrue:
				[self nilReadBeforeWrittenTemps]]
		ifFalse: "if optimized this isn't an actual scope"
			[rootNode noteBlockExit:
				[:exitNumber|
				 blockExtent := blockStart to: exitNumber]].

	"Now that the analysis is done move any temps that need to be moved."
	self postNumberingProcessTempsWithin: effectiveScope rootNode: rootNode.

	"This is simply a nicety for compiler developers..."
	temporaries do:
		[:temp|
		(temp isIndirectTempVector and: [temp name includes: $?]) ifTrue:
			[temp name: temp definingScope remoteTempNodeName]]