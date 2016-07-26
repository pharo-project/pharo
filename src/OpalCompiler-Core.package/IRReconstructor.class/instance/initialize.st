initialization
initialize
	temps := Dictionary new.
	remoteTemps := Dictionary new.
	closureCopiedValues := Dictionary new.
	sourceMapByteIndex := 0.
	super initialize.