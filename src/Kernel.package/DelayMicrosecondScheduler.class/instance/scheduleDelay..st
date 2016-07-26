timer process
scheduleDelay: aDelay
	"Private. Schedule this Delay."
	
	aDelay schedulerBeingWaitedOn: true.
	activeDelay 
		ifNil: [ activeDelay := aDelay ]
		ifNotNil: [
			aDelay schedulerResumptionTime < activeDelay schedulerResumptionTime
				ifTrue: [
					suspendedDelays add: activeDelay.
					activeDelay := aDelay ]
				ifFalse: [ suspendedDelays add: aDelay ]]
