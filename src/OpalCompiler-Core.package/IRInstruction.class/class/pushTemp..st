instance creation
pushTemp: aName
	aName = 'self' ifTrue: [self error: 'use pushReceiver'].
	^ IRPushTemp new
		name: aName;
		yourself