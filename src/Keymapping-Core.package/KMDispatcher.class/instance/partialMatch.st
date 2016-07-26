match
partialMatch
	| event |
	KMBuffer uniqueInstance partialMatch.
	event := KMBuffer uniqueInstance currentEvent.
	event isKeyboard not ifTrue: [ event := nil ].
	self announcer announce: (KMPartialMatch event: event from: self).