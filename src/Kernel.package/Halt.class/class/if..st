halting
if: condition
	<debuggerCompleteToSender>
	"This is the typical message to use for inserting breakpoints during 
	debugging.
	
	The argument can be one of the following:
		- a block: if the Block has one arg, the calling object is bound to that.
		- an expression
 		- a selector: Halt if found in the call chain"
	LocalRecursionStopper during: [ 
		condition isSymbol ifTrue: [ 
			^ self haltIfCallChain: thisContext home sender contains: condition ].
		condition isBlock ifTrue: [ 
			^ self haltIfBlock: condition withCallingObjectFrom: thisContext home sender].
		condition ifTrue: [self signalIn: thisContext home sender ].
	]