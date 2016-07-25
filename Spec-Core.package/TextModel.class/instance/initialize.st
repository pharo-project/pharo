initialization
initialize
	super initialize.
	textHolder := '' asValueHolder.
	behaviorHolder := nil asValueHolder.
	aboutToStyleHolder := [ false ] asValueHolder.
	actionToPerformHolder := [ :text |  ] asValueHolder.
	readSelectionHolder := [ selection value ] asValueHolder.
	actionPerformedHolder := '' asValueHolder.
	isCodeCompletionAllowedHolder := false asValueHolder.
	isForSmalltalkCodeHolder := false asValueHolder.
	doItContext := nil asValueHolder.
	doItReceiver := nil asValueHolder.
	hasUnacceptedEdits := false asValueHolder.
	selection := (1 to: 0) asValueHolder.
	scrollValue := (0 @ 0) asValueHolder.
	menuHolder := [ :menu :shifted |  ] asValueHolder.
	autoAccept := false asValueHolder.
	wantsVisualFeedback := true asValueHolder.
	askBeforeDiscardingEdits := true asValueHolder.
	self beForText.
	self registerEvents.
	self registerEventsForShout