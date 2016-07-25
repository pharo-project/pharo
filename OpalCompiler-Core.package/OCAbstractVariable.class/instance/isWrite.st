read/write usage
isWrite
	^ usage = #write or: [ self isRepeatedWrite ]