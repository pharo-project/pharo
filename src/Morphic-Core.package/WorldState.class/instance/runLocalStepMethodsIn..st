stepping
runLocalStepMethodsIn: aWorld 
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world.
	ar 3/13/1999: Remove buggy morphs from the step list so that they don't raise repeated errors."

	| now morphToStep stepTime priorWorld |
	now := Time millisecondClockValue.
	priorWorld := ActiveWorld.
	ActiveWorld := aWorld.
	self triggerAlarmsBefore: now.
	stepList isEmpty 
		ifTrue: 
			[ActiveWorld := priorWorld.
			^self].
	(now < lastStepTime or: [now - lastStepTime > 5000]) 
		ifTrue: [self adjustWakeupTimes: now].	"clock slipped"
	[stepList isEmpty not and: [stepList first scheduledTime < now]] 
		whileTrue: 
			[lastStepMessage := stepList removeFirst.
			morphToStep := lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: aWorld) 
				ifTrue: 
					[lastStepMessage value: now.
					lastStepMessage ifNotNil: 
							[stepTime := lastStepMessage stepTime ifNil: [morphToStep stepTime].
							lastStepMessage scheduledTime: now + (stepTime max: 1).
							stepList add: lastStepMessage]].
			lastStepMessage := nil].
	lastStepTime := now.
	ActiveWorld := priorWorld