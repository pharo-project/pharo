announcement handling
methodRecategorized: aMethodRecategorized 

	| oldMethod newMethod |
	"Workaround: In Pharo3, when a trait is removed from the trait composition, then this announcement is wrongly announced."
	aMethodRecategorized methodRecategorized ifNil: [ ^ self ].

	oldMethod := aMethodRecategorized methodRecategorized asEpiceaRingDefinition
		protocol: aMethodRecategorized oldProtocol;
		yourself.
	newMethod := aMethodRecategorized methodRecategorized asEpiceaRingDefinition
		protocol: aMethodRecategorized newProtocol;
		yourself.
	 
	self addEvent: 
		(EpMethodModification
			oldMethod: oldMethod
			newMethod: newMethod)