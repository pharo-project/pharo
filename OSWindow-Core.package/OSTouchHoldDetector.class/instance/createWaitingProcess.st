creation
createWaitingProcess
	^ [ 
	(Delay forMilliseconds: waitingTime) wait.
	(self activeFingers allSatisfy: [ :each | each totalDistanceTravelled < maxDistanceTravelled ])
		ifTrue: [ self sendToEventHandler: self createHoldEvent ] ] fork