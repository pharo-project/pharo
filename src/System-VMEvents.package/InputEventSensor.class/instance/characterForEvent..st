private events
characterForEvent: evtBuf

	| keycode |
	evtBuf ifNil: [^nil].
	keycode := evtBuf sixth.
	^keycode
		ifNotNil: [Unicode value: keycode]