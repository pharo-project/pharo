Extension { #name : 'Mutex' }

{ #category : '*Calypso-NavigationModel' }
Mutex >> tryEnterCritical: criticalBlock ifLocked: lockedBlock [

	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess == self owner ifTrue: [ ^ criticalBlock value ].
	^ self semaphore
		  critical: [
				  self owner: activeProcess.
				  criticalBlock ensure: [ self owner: nil ] ]
		  ifLocked: lockedBlock
]
