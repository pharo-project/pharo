events
notifyMorphsOfEvent: anEvent ofType: eventType from: sourceMorph
	| result |
	result := false.
	((subscriptions includesKey: eventType) not or: [ (subscriptions at: eventType) isEmpty ])
		ifTrue: [ ^false ].
	(subscriptions at: eventType) do: [ :s | result := result | ((s notify: anEvent from: sourceMorph) == true) ].
	^result