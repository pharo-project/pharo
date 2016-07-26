scanning
hasInstVarRef
	"Answer whether the method references an instance variable."

	| scanner end printer |

	scanner := InstructionStream on: self.
	printer := InstVarRefLocator new.
	end := self endPC.

	[scanner pc <= end] whileTrue: [
		(printer interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false