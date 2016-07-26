match
completeMatch: aKeymapEntry buffer: aBuffer
	KMLog logCompleteMatchBetween: morph and: aKeymapEntry.
	KMBuffer uniqueInstance completeMatch.
	self announcer announce: (KMCompleteMatch event: currentEvent from: self).