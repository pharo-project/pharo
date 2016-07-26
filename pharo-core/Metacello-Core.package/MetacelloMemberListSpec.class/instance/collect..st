enumeration
collect: aBlock 
	| newCollection |
	newCollection :=OrderedCollection new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^ newCollection