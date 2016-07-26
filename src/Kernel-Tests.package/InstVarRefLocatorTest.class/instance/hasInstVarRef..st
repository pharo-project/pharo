private
hasInstVarRef: aMethod
	"Answer whether the receiver references an instance variable."

	| scanner end printer |

	scanner := InstructionStream on: aMethod.
	printer := InstVarRefLocator new.
	end := scanner method endPC.

	[scanner pc <= end] whileTrue: [
		(printer interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false