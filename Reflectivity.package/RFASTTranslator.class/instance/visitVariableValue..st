reflectivity
visitVariableValue: aVariable
	self flag: #TBD. "needs to be extendend to other kinds of variables and cleaned"
	((aVariable isKindOf: LiteralVariable) or: [ (aVariable isKindOf: Slot) ]) ifTrue: [ self emitMetaLinkBefore: aVariable. ].
	aVariable emitValue: methodBuilder.
	((aVariable isKindOf: LiteralVariable) or: [ (aVariable isKindOf: Slot) ]) ifTrue: [self emitMetaLinkAfterNoEnsure: aVariable].