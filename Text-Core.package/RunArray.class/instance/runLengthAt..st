accessing
runLengthAt: index 
	"Answer the length remaining in run beginning at index."

	self at: index 
		setRunOffsetAndValue: [:run :offset :value | ^(runs at: run) - offset]