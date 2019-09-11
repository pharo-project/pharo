Extension { #name : #Mutex }

{ #category : #'*Calypso-NavigationModel' }
Mutex >> tryEnterCritical: criticalBlock ifLocked: lockedBlock [

	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess == owner ifTrue:[^criticalBlock value].
	^semaphore critical:[
		owner := activeProcess.
		criticalBlock ensure:[owner := nil]] ifLocked: lockedBlock
]
