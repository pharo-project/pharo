*OpalCompiler-Core
neighborPCWithCorrectMapping
	"Answer a pc inside the enclosing block or mathod that is correctly mapped to an AST node"
	"This is an ugly and temporary fix for Pharo 3. 
	Must be removed as soon as mappings are fixed"
	| neighborPC |
	neighborPC := self isDead 
		ifTrue: [ self startpc ]
		ifFalse: [ pc ].
	"There is a pushLiteral: nil bytecode for each temps in a block. 
	There is a 'pushTemp:' bytecode for each copied value of a block.
	These bytecodes are not mapped to any IR.
	We skip both"
	[ self isPushLiteralNil: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	[ self isPushTemp: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	"The block return ir instruction is mapped to the block instead of the sequence AST node. So we go just before it"
	(self isBlockReturn: neighborPC) ifTrue: [ neighborPC := neighborPC - 1 ].
	^ neighborPC