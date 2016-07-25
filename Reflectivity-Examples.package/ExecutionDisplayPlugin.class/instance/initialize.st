initialization
initialize

	super initialize.
	morph := ExecutionDisplayMorph new height: 20.
	link := MetaLink new metaObject: morph; selector: #noteExecuted.