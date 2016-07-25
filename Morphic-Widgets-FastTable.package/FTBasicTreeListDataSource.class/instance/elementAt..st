private
elementAt: index
	| curr b |
	
	curr := 0.

	b := nil.
	b := [ :p | p do: [ :each | (curr := curr + 1) = index ifTrue: [ ^each ] ifFalse: [ b value: each children ] ] ].
	
	b value: rootItems.

	SubscriptOutOfBounds signal: index.