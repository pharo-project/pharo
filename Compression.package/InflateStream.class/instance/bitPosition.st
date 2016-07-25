bit access
bitPosition
	"Return the current bit position of the source"
	sourceStream == nil
		ifTrue:[^sourcePos * 8 + bitPos]
		ifFalse:[^sourceStream position + sourcePos * 8 + bitPos]