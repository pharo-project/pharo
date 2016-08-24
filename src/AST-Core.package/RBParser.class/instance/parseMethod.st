private-parsing
parseMethod
	| methodNode | 
	methodNode := self parseMessagePattern.
	self parsePragmas.
	self addCommentsTo: methodNode.
	
	methodNode body: self sequenceNodeClass new.
	(self parseStatements: true into: methodNode body).
	
	pragmas ifNotNil: [ methodNode pragmas: pragmas ].
	^methodNode