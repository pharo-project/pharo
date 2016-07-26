code generation (closures)
addRemoteTemp: aTempVariableNode encoder: encoder
	remoteTemps ifNil:
		[remoteTemps := OrderedCollection new].
	remoteTemps addLast: aTempVariableNode.
	aTempVariableNode referenceScopesAndIndicesDo:
		[:scopeBlock "<BlockNode>" :location "<Integer>"|
		 self addReadWithin: scopeBlock at: location].
	encoder supportsClosureOpcodes ifFalse:
		[encoder encodeLiteral: remoteTemps size.
		 readNode := encoder encodeSelector: #at:.
		 writeNode := encoder encodeSelector: #at:put:]