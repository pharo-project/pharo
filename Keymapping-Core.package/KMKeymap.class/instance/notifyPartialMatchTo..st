matching
notifyPartialMatchTo: aListener
	| listeners |
	aListener isCollection ifTrue: [ listeners := aListener ] ifFalse: [ { aListener } ].
	listeners do: [ :l | l partialMatch ].