enumerating
intersection: aTimespan

	 "Return the Timespan both have in common, or nil"

	 | aBegin anEnd |
	 aBegin := self start max: aTimespan start.
	 anEnd := self end min: aTimespan end.
	 anEnd < aBegin ifTrue: [^nil].

	 ^ self class starting: aBegin ending: anEnd.
