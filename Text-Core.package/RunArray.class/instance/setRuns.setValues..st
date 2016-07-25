private
setRuns: newRuns setValues: newValues
	lastIndex := nil. "flush access cache"
	runs := newRuns asArray.
	values := newValues asArray.