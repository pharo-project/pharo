accessing
log

	log == nil
		ifTrue: [log := OrderedCollection new].
	^log