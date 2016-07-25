examples
shoutedScriptingTextArea
	"self shoutedScriptingTextArea" 

	ModelForShout := nil.
	RubEditingArea new
		beWrapped;
		model: self;
		beForSmalltalkScripting;
		width: 500;
		updateTextWith: self someSmalltalkScript;
		openInWorld