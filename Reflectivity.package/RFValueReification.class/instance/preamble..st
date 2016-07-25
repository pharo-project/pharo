generate
preamble: aNode
	(aNode isKindOf: RBProgramNode) ifFalse: [ ^#() ].
	^(aNode isReturn or: [ aNode isMessage])
		ifTrue: [RFStoreIntoTempNode named: #RFReifyValueVar]
		ifFalse: [ #() ]. 