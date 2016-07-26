accessing
someSmalltalkScript
	^ '" Enter some code here as in a workspace"' , String cr , 'RubTextArea new
		beWrapped;
		smalltalkScriptingModel: self;
		width: 500;
		setTextWith: ''" Enter some code here as in a workspace"'';
		openInWorld'