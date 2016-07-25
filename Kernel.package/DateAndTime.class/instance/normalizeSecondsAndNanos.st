private
normalizeSecondsAndNanos
	(NanosInSecond <= nanos or: [ nanos < 0 ])
		ifTrue: [
			seconds := seconds + (nanos // NanosInSecond).
			nanos := nanos \\ NanosInSecond].
	(SecondsInDay <= seconds or: [ seconds < 0 ])
		ifTrue: [
			julianDayNumber := julianDayNumber + (seconds // SecondsInDay).
			seconds := seconds \\ SecondsInDay].
