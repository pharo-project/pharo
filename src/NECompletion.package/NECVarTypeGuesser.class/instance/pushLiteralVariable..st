bytecode decoding
pushLiteralVariable: anAssociation 
	"Push Contents Of anAssociation On Top Of Stack bytecode."
	| info |
	contextCount > 0 ifTrue:[^self].

	info := NECTypeInfo definedByMessageSend: (anAssociation value isBehavior
		ifTrue: [ anAssociation value ]
		ifFalse: [ anAssociation value class ]).
	types add: info