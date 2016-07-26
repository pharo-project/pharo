testing
nextWakeUpTime
	^ accessProtect
		critical: [ activeDelay 
				ifNil: [ 0 ]
				ifNotNil: [ activeDelay schedulerResumptionTime ]]