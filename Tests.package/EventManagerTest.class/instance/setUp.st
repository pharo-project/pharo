running
setUp

	super setUp.
	eventSource := EventManager new.
	eventListener := Bag new.
	succeeded := false