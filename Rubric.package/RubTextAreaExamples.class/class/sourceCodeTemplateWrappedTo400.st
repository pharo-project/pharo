examples
sourceCodeTemplateWrappedTo400
	"self sourceCodeTemplateWrappedTo400"
	ModelForShout := self.
	RubEditingArea new
		model: self ;
		beForSmalltalkCode;
		beWrapped;
		width: 400;
		updateTextWith: self sourceCodeTemplate;
		openInWorld