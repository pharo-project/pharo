visitor-double dispatching
visitFullBlockNode: aBlockNode
	| compiledBlock copiedNames |
	compiledBlock := self compilationContext astTranslatorClass new translateFullBlock: aBlockNode.
	copiedNames := (aBlockNode scope inComingCopiedVars asArray collect: [:each | each name]).
	methodBuilder pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedNames