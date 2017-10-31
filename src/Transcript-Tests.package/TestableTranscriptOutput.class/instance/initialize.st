initialization
initialize

	data := String new writeStream.
	subs := OrderedCollection new.
	clearCount := 0.
	closeCount := 0.
	entryCount := 0.
	updateCount := 0.
	subscribedInCount := 0.