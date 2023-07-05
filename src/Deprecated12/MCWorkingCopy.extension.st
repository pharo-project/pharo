Extension { #name : #MCWorkingCopy }

{ #category : #'*Deprecated12' }
MCWorkingCopy class >> allManagers [

	self deprecated: 'Use #allWorkingCopies instead' transformWith: '`@rcv allManagers' -> '`@rcv allWorkingCopies'.
	^ self allWorkingCopies
]
