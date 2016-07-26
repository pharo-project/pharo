enumeration
specListSelect: aBlock 
	| newCollection |
	newCollection := OrderedCollection new.
	self specListDo: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection