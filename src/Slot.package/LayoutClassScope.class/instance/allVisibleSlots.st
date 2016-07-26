accessing
allVisibleSlots
	| result |
	result := parentScope allVisibleSlots.
	self do: [ :slot | 
		slot isVisible 
			ifTrue: [ result add: slot ]].
	^ result