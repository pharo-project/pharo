private-parsing
parseMethod
	| methodNode sequenceNode | 
	methodNode := self parseMessagePattern.
	self parsePragmas.
	self addCommentsTo: methodNode.
	
	sequenceNode:=  self sequenceNodeClass new.
	(self parseStatements: true into: sequenceNode).
methodNode body: sequenceNode .	
	pragmas ifNotNil: [ methodNode pragmas: pragmas ].
	^methodNode