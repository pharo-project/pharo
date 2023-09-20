Extension { #name : 'RGContainer' }

{ #category : '*Deprecated12' }
RGContainer class >> allManagers [

	self deprecated: 'Use #allWorkingCopies instead' transformWith: '`@rcv allManagers' -> '`@rcv allWorkingCopies'.
	^ self allWorkingCopies
]
