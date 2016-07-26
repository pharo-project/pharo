animating
animationStep
	|delta stepValue currentTime stop stepStamp|
	self needsStep: false.
	currentTime := Time millisecondClockValue.
	stop := currentTime >= self finishTime.
	delta := self duration = 0
		ifTrue: [ 1 ]
		ifFalse: [ ((currentTime - startTime) min: self duration) / self duration ].
	delta = 1 ifTrue: [ stop := true ].
	stepValue := stepLogic
		brickValue: self animatedObject
		withEnoughArguments: { self fromValue . self toValue . delta }.
	(delta = 1) | (stepValue = self toValue) ifTrue: [ stop := true ].
	stepStamp := stamp.
	UIManager default defer: [
		self asyncTask isTerminated not & (stepStamp = stamp) ifTrue: [
			steppedLogic brickValue: self animatedObject withEnoughArguments: { delta }.
			animationLogic brickValue: self animatedObject withEnoughArguments: { stepValue }.
			stop
				ifFalse: [ self needsStep: true. self asyncTask resume ]
				ifTrue: [ self shouldStop: true. self asyncTask resume ] ] ].
		
	self asyncTask pause
	
	