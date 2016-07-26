code generation (closures)
remoteTempNodeName
	"Answer a useful name for a RemoteTempVectorNode in the receiver."
	| prefix scope extent |
	prefix := actualScopeIfOptimized ifNil: ['<'] ifNotNil: [ '<...'].
	scope := self.
	[extent := scope blockExtent.
	 extent == nil
	 and: [scope actualScope ~~ scope]] whileTrue:
		[scope := scope actualScope].
	^extent
		ifNil: [prefix, '?-?>']
		ifNotNil:
			[prefix, extent first printString, '-',
				(extent last isZero
					ifTrue: ['?']
					ifFalse: [extent last printString]), '>']