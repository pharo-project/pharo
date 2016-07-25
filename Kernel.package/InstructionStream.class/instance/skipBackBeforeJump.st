scanning
skipBackBeforeJump
	"Assuming that the receiver is positioned just after a jump, skip back one or two bytes,
	 depending on the size of the previous jump instruction."
	| scanner client prevPc |
	scanner := InstructionStream on: self method.
	client := InstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	scanner pc: prevPc.
	(scanner willJumpIfTrue or: [scanner willJumpIfFalse]) ifFalse:
		[self error: 'Where''s the jump??'].
	self jump: prevPc - pc