instructions
pushClosureCopyCopiedValues: copiedValuesNames args: args jumpTo: aJumpLabel
	| anInstruction |
	anInstruction := super pushClosureCopyCopiedValues: copiedValuesNames args: args jumpTo: aJumpLabel.
	"remove pushed nils of local temps"
	anInstruction copiedValues withIndexDo: [ :aValue :index |
		self rememberReference: anInstruction -> index to: aValue in: closureCopiedValues ]