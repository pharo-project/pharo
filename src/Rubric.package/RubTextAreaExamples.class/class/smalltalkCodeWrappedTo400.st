examples
smalltalkCodeWrappedTo400
	"self smalltalkCodeWrappedTo400"
	| area |
	ModelForShout := self.
	area := RubEditingArea new
		beWrapped;
		model: self;
		width: 400;
		beForSmalltalkCodeWithDefaultFont;
		updateTextWith: self someSmalltalkCode.
	area openInWorld.
	^ area