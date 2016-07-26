halting
if: condition
	<debuggerCompleteToSender>
	"This is the typical message to use for inserting breakpoints during 
	debugging.
	
	The argument can be one of the following:
		- a block: if the Block has one arg, the calling object is bound to that.
		- an expression
 		- a selector: Halt if found in the call chain"

	condition isSymbol ifTrue: [ ^ self haltIfCallChainContains: condition ].
	condition isBlock ifTrue: [ ^ self haltIfBlockWithCallingObject: condition].
	condition ifTrue: [self signal].