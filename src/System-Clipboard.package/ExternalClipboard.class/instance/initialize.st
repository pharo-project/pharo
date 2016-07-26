initialization
initialize
	super initialize.
	clipboard := [self createClipboard] on: Error do: [:ex | clipboard := 0]