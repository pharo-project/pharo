accessing
slots
	| result |
	result := OrderedCollection new.
	self do: [ :slot |  result add: slot].
	^ result asArray