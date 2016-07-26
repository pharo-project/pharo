private
setJdn: julDays seconds: secs nano: nanoSecs offset: anOffset
	julianDayNumber := julDays.
	seconds := secs.
	nanos := nanoSecs.
	offset := anOffset.
	self normalizeSecondsAndNanos