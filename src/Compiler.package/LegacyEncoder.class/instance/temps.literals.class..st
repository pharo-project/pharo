initialize-release
temps: tempVars literals: lits class: cl 
	"Decompile."

	supered := false.
	class := cl.
	nTemps := tempVars size.
	tempVars do: [:node | scopeTable at: node name put: node].
	(literalStream := lits readStream) position: lits size.
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32.
