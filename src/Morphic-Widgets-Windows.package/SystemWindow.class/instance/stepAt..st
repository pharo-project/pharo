stepping
stepAt: millisecondClockValue
	"If the receiver is not collapsed, step it, after first stepping the model."

	(isCollapsed not or: [self wantsStepsWhenCollapsed]) ifTrue:
		[model ifNotNil: [model stepAt: millisecondClockValue in: self].
		super stepAt: millisecondClockValue "let player, if any, step"]

"Since this method ends up calling step, the model-stepping logic should not be duplicated there."