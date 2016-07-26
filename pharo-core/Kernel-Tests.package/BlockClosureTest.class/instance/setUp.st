setup
setUp
	super setUp.
	aBlockContext := [100@100 corner: 200@200].
	contextOfaBlockContext := thisContext.