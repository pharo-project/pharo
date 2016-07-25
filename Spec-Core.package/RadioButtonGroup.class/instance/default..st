api
default: aButton

	aButton privateSetState: true.
	aButton takeKeyboardFocus.
	aButton whenBuiltDo: [ aButton privateSetState: true ]