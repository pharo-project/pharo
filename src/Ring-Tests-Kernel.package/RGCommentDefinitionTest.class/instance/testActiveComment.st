testing
testActiveComment

	| rgComment |
	rgComment := RGCommentDefinition realClass: RGClassDefinition.
	self assert: rgComment isActive.
	self assert: rgComment sourcePointer isNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil.
	rgComment content: ''.
	"it always reads from the organization and the previous changes was not commited"
	self assert: rgComment content ~= ''.
	
	rgComment fromActiveToPassive.
	self assert: rgComment isPassive.
	self assert: rgComment sourcePointer isNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil.
	rgComment content: ''.
	self assert: rgComment content = ''.
	
	rgComment := RGCommentDefinition realClass: RGClassDefinition.
	rgComment fromActiveToHistorical.
	self assert: rgComment isHistorical.
	self assert: rgComment sourcePointer notNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil.
	rgComment content: ''.
	"it always reads from the source file and the previous changes was not commited"
	self assert: rgComment content ~= ''