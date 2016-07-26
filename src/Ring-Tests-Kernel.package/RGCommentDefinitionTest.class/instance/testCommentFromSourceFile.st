testing
testCommentFromSourceFile
	| rgComment |
	
	rgComment := (RGCommentDefinition realClass: RGClassDefinition) asHistorical.
	self assert: rgComment isHistorical.
	self assert: rgComment sourcePointer notNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil