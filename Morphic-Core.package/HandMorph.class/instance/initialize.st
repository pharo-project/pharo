initialization
initialize
	super initialize.
	self initForEvents.
	keyboardFocus := nil.
	mouseFocus := nil.
	bounds := 0@0 extent: Cursor normal extent.
	damageRecorder := DamageRecorder new.
	cachedCanvasHasHoles := false.
	temporaryCursor := temporaryCursorOffset := nil.
	self initForEvents.