accessing
visibleSlots
	| result |
	result := OrderedCollection new.
	self do: [ :slot | 
		slot isVisible 
			ifTrue: [ result add: slot ]].
	^ result asArray