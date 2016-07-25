enumeration
select: aBlock 
	| newCollection |
	newCollection := OrderedCollection new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection