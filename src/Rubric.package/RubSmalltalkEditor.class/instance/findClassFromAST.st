menu messages
findClassFromAST
	"Try to make a class name out of the current text selection"

	| node fallbackBlock className |		
	
	"Preserve original behavior - although could consider building AST from selection"
	self hasSelection ifTrue: [ 
		^(self selection string copyWithoutAll: CharacterSet crlf) trimBoth ].
	
	fallbackBlock := [^nil].

	node := self bestNodeInTextAreaOnError: fallbackBlock.
	
	[node isVariable] whileFalse: [ 
	 	(node := node parent) ifNil: fallbackBlock ].
	
	className := node name.
	
	[ className first isUppercase ] whileFalse: [ 
		(className := className allButFirst) ifEmpty: fallbackBlock  ].
	
	^className